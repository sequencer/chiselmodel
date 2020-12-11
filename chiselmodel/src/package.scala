// SPDX-License-Identifier: Apache-2.0

import chisel3._
import chisel3.experimental.{annotate, requireIsHardware, ChiselAnnotation, RunFirrtlTransform}
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}
import chiselmodel.firrtl._
import firrtl.Transform
import firrtl.annotations.Annotation
import chiselmodel.macros.ValNameImpl

package object chiselmodel {
  implicit val sourceInfo: SourceInfo = UnlocatableSourceInfo

  /** Create a SystemVerilog DPI BlackBox for `data`.
    * @param data Data to be accessed by DPI.
    */
  def dpi(data: Data)(implicit valName: ValName): Unit = {
    dpi(data, when.cond, Module.clock, Module.reset, valName.name)
  }

  /** Create a SystemVerilog DPI BlackBox for `data`.
    * @param data Data to be accessed by DPI.
    */
  def dpi(data: Data, condition: Bool, clock: Clock, reset: Reset, name: String): Unit = {
    requireIsHardware(data)
    val dataName = data.computeName(None, None).get
    val conditionWire = WireDefault(condition).suggestName("DPI_CONDITION_" + dataName)
    val clockWire = WireDefault(clock).suggestName("DPI_CLOCK_" + dataName)
    val resetWire = WireDefault(reset).suggestName("DPI_RESET_" + dataName)
    data := DontCare
    dontTouch(conditionWire)
    dontTouch(clockWire)
    dontTouch(resetWire)
    annotate(new ChiselAnnotation with RunFirrtlTransform {
      override def toFirrtl: Annotation =
        DPIAnnotation(data.toTarget, conditionWire.toTarget, clockWire.toTarget, resetWire.toTarget, name)
      override def transformClass: Class[_ <: Transform] = classOf[DPITransform]
    })
  }

  case class ValName(name: String)

  object ValName {
    implicit def materialize(implicit x: ValNameImpl): ValName = ValName(x.name)
  }

}
