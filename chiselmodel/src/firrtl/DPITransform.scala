package chiselmodel.firrtl

import firrtl.Utils.swap
import firrtl._
import firrtl.analyses.{CircuitGraph, IRLookup}
import firrtl.annotations.TargetToken
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.passes._
import firrtl.stage.TransformManager.TransformDependency

/** DPITransform to add COSIM blackbox to DPI Modules.
  *
  * @throws DPIAnnotatedToNoPortTargetException if annotate to a Target not being a Port.
  */
class DPITransform extends Transform with DependencyAPIMigration {
  // [[firrtl.analyses.ConnectionGraph]] should run before [[firrtl.stage.Forms.HighForm]].
  override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.LowForm

  override def optionalPrerequisiteOf: Seq[TransformDependency] = Seq(
    // this transform will remove [[IsInvalid]] statement of annotated Data.
    Dependency(RemoveValidIf)
  )

  override def invalidates(a: Transform): Boolean = a match {
    case ToWorkingIR | InferTypes | ResolveFlows | ExpandConnects => true
    case _                                                        => false
  }

  override protected def execute(state: CircuitState): CircuitState = {

    /** circuit graph for analysing fan-in and fan-out. */
    val circuitGraph = CircuitGraph(state.circuit)

    /** map to convert Target to statement and expression. */
    val irLookup = IRLookup(state.circuit)

    /** gather all annotated [[DPIAnnotation]]. */
    val dpiAnnotations = state.annotations.collect { case a: DPIAnnotation => a }

    /** If a [[DefModule]] has [[DPIAnnotation]], modify it by
      * adding new DPI [[ExtModule]] instance;
      * connecting DPI data to DPI instance;
      * removing all [[IsInvalid]] statement for annotated signals.
      *
      * If a [[DefModule]] has not [[DPIAnnotation]], directly return it.
      *
      * @return [[DefModule]] contains modified [[DefModule]] and auto generated DPI ExtModule.
      */
    def addDPIToModule(defModule: DefModule): Seq[DefModule] = {
      defModule match {
        case extModule: ExtModule => Seq(extModule)
        case module:    Module =>
          dpiAnnotations.filter(_.module.module == module.name) match {
            case Nil => Seq(module)
            case moduleDpiAnnotations =>
              val (extModules, statementToBeAdded, statementToBeRemoved) = moduleDpiAnnotations
                // for each [[DPIAnnotation.name]] construct a [[ExtModule]]
                .groupBy(_.name)
                .map {
                  // convert DPIAnnotation to port in the `module`
                  case (name, dpiAnnotations) =>
                    // make sure only a DPI Annotation can be annotated to one instance.
                    val pathGroups: Map[Seq[(TargetToken.Instance, TargetToken.OfModule)], Seq[DPIAnnotation]] =
                      dpiAnnotations.groupBy(_.targets.flatMap(_.flatMap(_.path)).distinct)
                    if (pathGroups.size != 1) {
                      throw new DPINameCollapsedException(pathGroups)
                    }
                    // make sure annotation of one name shares same clock, reset, condition
                    val (conditions, clocks, resets) = dpiAnnotations.map {
                      case DPIAnnotation(_, condition, clock, reset, _) =>
                        (
                          circuitGraph.fanInSignals(condition),
                          circuitGraph.fanInSignals(clock),
                          circuitGraph.fanInSignals(reset)
                        )
                    }.unzip3
                    // @todo create correspond exceptions
                    require(conditions.flatten.distinct.size == 1)
                    require(clocks.flatten.distinct.size == 1)
                    require(resets.flatten.distinct.size == 1)

                    // @todo check [[dpiAnnotations.data]] has not other sink and source

                    // construct names
                    val extModuleName = s"${name}_${dpiAnnotations.head.data.path.map(_._1.value).mkString("_")}"
                    val extModuleInstance = DefInstance(extModuleName, extModuleName)

                    // @todo check types
                    val clockType: Type = irLookup.tpe(dpiAnnotations.head.clock)
                    val resetType = irLookup.tpe(dpiAnnotations.head.clock)
                    val conditionType = irLookup.tpe(dpiAnnotations.head.clock)

                    val (extModulePorts, newConnections) = dpiAnnotations.map {
                      case DPIAnnotation(data, condition, clock, reset, _) =>
                        val dataDirection: Direction = (irLookup.declaration(data) match {
                          case p: Port => p
                          // if not annotate to a [[Port]], throw [[DPIAnnotatedToNoPortTargetException]]
                          case _ => throw new DPIAnnotatedToNoPortTargetException(data)
                        }).direction
                        (
                          Seq(
                            Port(NoInfo, "clock", Input, clockType),
                            Port(NoInfo, "reset", Input, resetType),
                            Port(NoInfo, "condition", Input, conditionType),
                            Port(
                              NoInfo,
                              // @todo need to handle SubIndex and SubVec (sanitize name)
                              data.name,
                              // get the port direction and flip it.
                              swap(dataDirection),
                              irLookup.tpe(data)
                            )
                          ),
                          Seq(
                            Connect(
                              NoInfo,
                              SubField(Reference(extModuleInstance), "clock"),
                              irLookup.expr(clock)
                            ),
                            Connect(
                              NoInfo,
                              SubField(Reference(extModuleInstance), "reset"),
                              irLookup.expr(reset)
                            ),
                            Connect(
                              NoInfo,
                              SubField(Reference(extModuleInstance), "condition"),
                              irLookup.expr(condition)
                            ),
                            dataDirection match {
                              case Output =>
                                Connect(
                                  NoInfo,
                                  irLookup.expr(data),
                                  SubField(Reference(extModuleInstance), data.name)
                                )
                              case Input =>
                                Connect(
                                  NoInfo,
                                  SubField(Reference(extModuleInstance), data.name),
                                  irLookup.expr(data)
                                )
                            }
                          )
                        )
                    }.unzip
                    (
                      ExtModule(
                        info = NoInfo,
                        name = extModuleName,
                        ports = extModulePorts.flatten.distinct,
                        defname = extModuleName,
                        params = Seq.empty
                      ),
                      extModuleInstance +: newConnections.flatten,
                      dpiAnnotations
                        .flatMap(annotation =>
                          circuitGraph.fanOutSignals(annotation.data) ++ circuitGraph.fanInSignals(annotation.data)
                        )
                        .map(irLookup.declaration)
                        .filter(_.isInstanceOf[IsInvalid])
                    )
                }
                .unzip3
              Seq(
                module.copy(body =
                  Block(
                    module.body.mapStmt {
                      case statement if statementToBeRemoved.flatten.toSet.contains(statement) => EmptyStmt
                      case statement                                                           => statement
                    },
                    statementToBeAdded.flatten.toSeq.distinct: _*
                  )
                )
              ) ++ extModules
          }
      }
    }
    // construct new state with modifying all Module with DPIAnnotation and add auto-generated [[dpiExtModules]]
    state.copy(circuit = state.circuit.copy(modules = state.circuit.modules.flatMap(addDPIToModule)))
  }
}
