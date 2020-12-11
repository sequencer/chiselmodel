package chiselmodel.firrtl

import firrtl.Utils.swap
import firrtl._
import firrtl.analyses.{CircuitGraph, ConnectionGraph, IRLookup, InstanceKeyGraph}
import firrtl.annotations.TargetToken
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.passes.{ExpandConnects, InferTypes, ResolveFlows, ToWorkingIR}
import firrtl.stage.TransformManager.TransformDependency

/** DPITransform to add COSIM blackbox to DPI Modules.
  *
  * @throws DPIAnnotatedToNoPortTargetException if annotate to a Target not being a Port.
  */
class DPITransform extends Transform with DependencyAPIMigration {
  // need [[FixFlows]] to fix auto-generated connection.
  override val optionalPrerequisiteOf = Seq(
    Dependency[VerilogEmitter],
    Dependency(FixFlows)
  )

  // [[firrtl.analyses.ConnectionGraph]] should run before [[firrtl.stage.Forms.HighForm]].
  override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.LowForm

  override def invalidates(a: Transform): Boolean = a match {
    case ToWorkingIR | InferTypes | ResolveFlows | ExpandConnects => true
    case _                                                        => false
  }

  override protected def execute(state: CircuitState): CircuitState = {

    val circuitGraph = CircuitGraph(state.circuit)

    val connectionGraph = ConnectionGraph(state.circuit)

    /** use [[firrtl.analyses.ConnectionGraph]] to to find ir annotated by [[dpiAnnotations]]. */

    /** gather all annoated [[DPIAnnotation]]. */
    val dpiAnnotations = state.annotations.collect { case a: DPIAnnotation => a }

    def addDPIToModule(defModule: DefModule): Seq[DefModule] = {
      defModule match {
        case extModule: ExtModule => Seq(extModule)
        case module:    Module    =>
          /** All [[DPIAnnotation]] of [[module]] */
          val moduleDpiAnnotations = dpiAnnotations.filter(_.module.module == module.name)

          // modify `module` if it contains [[DPIAnnotation]]
          if (moduleDpiAnnotations.nonEmpty) {
            val (extModules, statements) = moduleDpiAnnotations
              .groupBy(_.name)
              .map {
                // convert DPIAnnotation to port in the `module`
                case (name, dpiAnnotations) => {
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

                  // construct names
                  val extModuleName = s"${name}_${dpiAnnotations.head.data.path.map(_._1.value).mkString("_")}"
                  val extModuleInstance = DefInstance(extModuleName, extModuleName)

                  // @todo check types
                  val clockType: Type = connectionGraph.irLookup.tpe(dpiAnnotations.head.clock)
                  val resetType = connectionGraph.irLookup.tpe(dpiAnnotations.head.clock)
                  val conditionType = connectionGraph.irLookup.tpe(dpiAnnotations.head.clock)

                  val (extModulePorts, newConnections) = dpiAnnotations.map {
                    case DPIAnnotation(data, condition, clock, reset, _) =>
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
                            swap((connectionGraph.irLookup.declaration(data) match {
                              case p: Port => p
                              // if not annotate to a [[Port]], throw [[DPIAnnotatedToNoPortTargetException]]
                              case _ => throw new DPIAnnotatedToNoPortTargetException(data)
                            }).direction),
                            connectionGraph.irLookup.tpe(data)
                          )
                        ),
                        Seq(
                          Connect(
                            NoInfo,
                            SubField(Reference(extModuleInstance), "clock"),
                            connectionGraph.irLookup.expr(clock)
                          ),
                          Connect(
                            NoInfo,
                            SubField(Reference(extModuleInstance), "reset"),
                            connectionGraph.irLookup.expr(reset)
                          ),
                          Connect(
                            NoInfo,
                            SubField(Reference(extModuleInstance), "condition"),
                            connectionGraph.irLookup.expr(condition)
                          ),
                          Connect(
                            NoInfo,
                            SubField(Reference(extModuleInstance), data.name),
                            connectionGraph.irLookup.expr(data)
                          )
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
                    extModuleInstance +: newConnections.flatten
                  )
                }
              }
              .unzip
            Seq(module.copy(body = Block(module.body, statements.flatten.toSeq.distinct: _*))) ++ extModules
          } else
            Seq(module)
      }
    }
    // construct new state with modifying all Module with DPIAnnotation and add auto-generated [[dpiExtModules]]
    state.copy(
      circuit = state.circuit.copy(
        modules = state.circuit.modules.flatMap(addDPIToModule)
      ),
      annotations = state.annotations.filterNot(_.isInstanceOf[DPIAnnotation])
    )
  }
}
