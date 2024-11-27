import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })
  val xReg = Reg(UInt (16.W))
  val yReg = Reg(UInt (16.W))
  val addressReg = Reg(UInt (16.W))
  val dataReg = Reg(UInt (32.W))

  val idle :: read :: write :: done :: init :: outerLoop :: border :: erode :: pixelRead :: rightRead :: leftRead :: upRead :: downRead :: white :: increment :: Nil = Enum (15)

  val stateReg = RegInit(idle)

  io.writeEnable := false.B
  io.dataWrite := dataReg
  io.address := 0.U
  io.done := false.B

  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := init
      }
    }
    is(init) {
      xReg := 0.U
      yReg := 0.U
      stateReg := outerLoop
    }
    is(outerLoop) {
      when (xReg < 20.U) {
        addressReg := xReg
        stateReg := border
      } .otherwise {
        stateReg := done
      }
    }
    is(border) {
      when (xReg === 0.U || xReg === 19.U || yReg === 0.U || yReg === 19.U) {
        stateReg := erode
      } .otherwise {
        stateReg := pixelRead
      }
    }
    is(pixelRead) {
      io.address := addressReg
      dataReg := io.dataRead
      when (dataReg === 255.U) {
        stateReg := rightRead
      } .otherwise {
        stateReg := erode
      }
    }
    is(rightRead) {
      io.address := addressReg + 1.U
      when (dataReg === 255.U) {
        stateReg := leftRead
      } .otherwise {
        stateReg := erode
      }
    }
    is(leftRead) {
      io.address := addressReg - 1.U
      when (dataReg === 255.U) {
        stateReg := upRead
      } .otherwise {
        stateReg := erode
      }
    }
    is(upRead) {
      io.address := addressReg + 20.U
      when (dataReg === 255.U) {
        stateReg := downRead
      } .otherwise {
        stateReg := erode
      }
    }
    is(downRead) {
      io.address := addressReg - 20.U
      when (dataReg === 255.U) {
        stateReg := white
      } .otherwise {
        stateReg := erode
      }
    }
    is(white) {
      io.writeEnable := true.B
      io.address := addressReg + 400.U
      stateReg := increment
    }
    is(erode) {
      io.writeEnable := true.B
      io.address := addressReg + 400.U
      dataReg := 0.U
      stateReg := increment
    }
    is(increment) {
      yReg := yReg + 1.U
      when(yReg < 20.U) {
        addressReg := addressReg + 20.U
        stateReg := border
      } .otherwise {
        xReg := xReg + 1.U
        yReg := 0.U
        stateReg := outerLoop
      }
    }
    is(done) {
      io.done := true.B
    }
  }
}
