package chiselmodel.firrtl

import firrtl._
import firrtl.ir._

import scala.collection.mutable.ListBuffer

/** DPITransform to add COSIM blackbox to DPI Modules.s
  */
class DPITransform extends Transform with DependencyAPIMigration {
  override protected def execute(state: CircuitState): CircuitState = {
    val dpiAnnotations = state.annotations.collect { case a: DPIAnnotation => a }

    /** constructed DPI [[ExtModule]], dynamically added by [[addDPIToModule]] when traverse all modules. */
    val dpiExtModules = ListBuffer[ExtModule]()

    def addDPIToModule(defModule: DefModule): DefModule = {
      // collect all annotations of [[defModule]]
      val defModuleDpiAnnotations = dpiAnnotations.filter(_.module.module == defModule.name)
      // modify [[defModule]] if it contains [[DPIAnnotation]]
      if (defModuleDpiAnnotations.nonEmpty) {
        // make sure only Ports can be annotated.

        // warning if There are undriven signals

        // group DPIs guarded by same condition and clocked same clock.

        // construct the cosim [[ExtModule]] with each group of DPIs.

        // add [[ExtModule]] instance to [[defModule]], and add related metadata to [[]]

        // connect signals to [[ExtModule]]
        defModule
      } else
        defModule
    }

    // construct new state with modifying all Module with DPIAnnotation and add auto-generated [[dpiExtModules]]
    state.copy(
      circuit = state.circuit.copy(
        modules = state.circuit.modules.map(addDPIToModule) ++ dpiExtModules
      )
    )
  }
}
