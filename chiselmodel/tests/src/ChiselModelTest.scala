package chiselmodelTests

import chisel3._
import chiselmodel.dpi
import chiselmodelTests.ChiselModelTest.GCDModel
import org.scalatest.flatspec.AnyFlatSpec

object ChiselModelTest {

  class GCDIO extends Bundle {
    val value1:      UInt = Input(UInt(16.W))
    val value2:      UInt = Input(UInt(16.W))
    val load:        Bool = Input(Bool())
    val outputGCD:   UInt = Output(UInt(16.W))
    val outputValid: Bool = Output(Bool())
  }

  class GCD extends MultiIOModule {
    val io = IO(new GCDIO)

    val x: UInt = Reg(UInt())
    val y: UInt = Reg(UInt())

    when(x > y) {
      x := x - y
    }.otherwise {
      y := y - x
    }

    when(io.load) {
      x := io.value1
      y := io.value2
    }

    io.outputGCD := x
    io.outputValid := y === 0.U
  }

  class GCDModel extends MultiIOModule {
    val io = IO(new GCDIO)
    dpi(io)
  }

}

class ChiselModelTest extends AnyFlatSpec {
  "GCDModel" should "emit" in {
    chisel3.stage.ChiselStage.emitVerilog(new GCDModel)
  }
}
