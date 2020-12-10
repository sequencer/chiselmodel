// SPDX-License-Identifier: Apache-2.0

import chisel3._
import chisel3.experimental.{annotate, requireIsHardware, ChiselAnnotation, RunFirrtlTransform}
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}
import chiselmodel.firrtl._
import firrtl.Transform
import firrtl.annotations.Annotation

package object chiselmodel {
  implicit val sourceInfo: SourceInfo = UnlocatableSourceInfo

  /** Create a SystemVerilog DPI BlackBox for `data`.
    * @param data Data to be accessed by DPI.
    */
  def dpi(data: Data): Unit = {
    dpi(data, when.cond, Module.clock, Module.reset)
  }

  /** Create a SystemVerilog DPI BlackBox for `data`.
    * @param data Data to be accessed by DPI.
    */
  def dpi(data: Data, condition: Bool, clock: Clock, reset: Reset): Unit = {
    requireIsHardware(data)

    val conditionWire = WireDefault(condition).suggestName("DPI_CONDITION_" + data.computeName(None, None).get)
    val clockWire = WireDefault(clock).suggestName("DPI_CLOCK_" + data.computeName(None, None).get)
    val resetWire = WireDefault(reset).suggestName("DPI_RESET_" + data.computeName(None, None).get)
    data := DontCare
    dontTouch(data)

    dontTouch(conditionWire)
    dontTouch(clockWire)
    dontTouch(resetWire)
    annotate(new ChiselAnnotation with RunFirrtlTransform {
      override def toFirrtl: Annotation =
        DPIAnnotation(data.toTarget, conditionWire.toTarget, clockWire.toTarget, resetWire.toTarget)
      override def transformClass: Class[_ <: Transform] = classOf[DPITransform]
    })
  }

}
