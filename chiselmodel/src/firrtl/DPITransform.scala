package chiselmodel.firrtl

import firrtl.Utils.swap
import firrtl._
import firrtl.analyses.IRLookup
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.passes.{ExpandConnects, InferTypes, ResolveFlows, ToWorkingIR}
import firrtl.stage.TransformManager.TransformDependency

import scala.collection.mutable.ListBuffer

/** DPITransform to add COSIM blackbox to DPI Modules.
  *
  * @throws DPIAnnotatedToNoPortTargetException if annotate to a Target not being a Port.
  */
class DPITransform extends Transform with DependencyAPIMigration {
  // need [[FixFlows]] to fix auto-generated connection.
  override val optionalPrerequisiteOf = Seq(
    Dependency(FixFlows)
  )

  // [[firrtl.analyses.ConnectionGraph]] should run before [[firrtl.stage.Forms.HighForm]].
  override def prerequisites: Seq[TransformDependency] = firrtl.stage.Forms.LowForm

  override def invalidates(a: Transform): Boolean = a match {
    case ToWorkingIR | InferTypes | ResolveFlows | ExpandConnects => true
    case _                                                        => false
  }

  override protected def execute(state: CircuitState): CircuitState = {

    /** use [[firrtl.analyses.ConnectionGraph]] to to find ir annotated by [[dpiAnnotations]]. */
    val irLookUp: IRLookup = IRLookup(state.circuit)

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
            val (extModules, statements) = moduleDpiAnnotations.map {
              // convert DPIAnnotation to port in the `module`
              case DPIAnnotation(data, condition, clock, reset) =>
                /** `extModule` name, `defname` and `name` are same since we will construct our own [[ExtModule]]. */
                val extModuleName = s"DPI_${data.module}_${irLookUp.expr(condition).serialize}".replace(".", "_")
                val extModule = ExtModule(
                  info = NoInfo,
                  name = extModuleName,
                  ports = Seq(
                    Port(NoInfo, "clock", Input, irLookUp.tpe(clock)),
                    Port(NoInfo, "reset", Input, irLookUp.tpe(reset)),
                    Port(NoInfo, "condition", Input, irLookUp.tpe(condition)),
                    Port(
                      NoInfo,
                      "io",
                      // get the port direction and flip it.
                      swap((irLookUp.declaration(data) match {
                        case p: Port => p
                        // if not annotate to a [[Port]], throw [[DPIAnnotatedToNoPortTargetException]]
                        case _ => throw new DPIAnnotatedToNoPortTargetException(data)
                      }).direction),
                      irLookUp.tpe(data)
                    )
                  ),
                  defname = extModuleName,
                  params = Seq.empty
                )
                val extModuleInstance = DefInstance(extModuleName, extModuleName)
                (
                  extModule,
                  Seq(
                    extModuleInstance,
                    Connect(NoInfo, SubField(Reference(extModuleInstance), "clock"), irLookUp.expr(clock)),
                    Connect(NoInfo, SubField(Reference(extModuleInstance), "reset"), irLookUp.expr(reset)),
                    Connect(NoInfo, SubField(Reference(extModuleInstance), "condition"), irLookUp.expr(condition)),
                    Connect(NoInfo, SubField(Reference(extModuleInstance), "io"), irLookUp.expr(data))
                  )
                )
            }.unzip
            Seq(module.copy(body = Block(module.body, statements.flatten: _*))) ++ extModules
          } else
            Seq(module)
      }
    }
    // construct new state with modifying all Module with DPIAnnotation and add auto-generated [[dpiExtModules]]
    state.copy(
      circuit = state.circuit.copy(
        modules = state.circuit.modules.flatMap(addDPIToModule)
      )
    )
  }
}
