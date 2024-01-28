// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._
import dinocpu.components._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends BaseCPU {
  // All of the structures required
  val pc              = dontTouch(RegInit(0.U(64.W)))
  val control         = Module(new Control())
  val registers       = Module(new RegisterFile())
  val aluControl      = Module(new ALUControl())
  val alu             = Module(new ALU())
  val immGen          = Module(new ImmediateGenerator())
  val jumpDetection   = Module(new JumpDetectionUnit())
  val jumpPcGen       = Module(new JumpPcGeneratorUnit())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  control.io := DontCare
  registers.io := DontCare
  aluControl.io := DontCare
  alu.io := DontCare
  immGen.io := DontCare
  jumpDetection.io := DontCare
  jumpPcGen.io := DontCare
  io.dmem <> DontCare

  //FETCH
  io.imem.address := pc
  io.imem.valid := true.B

  val instruction = Wire(UInt(32.W))
  when ((pc % 8.U) === 4.U) {
    instruction := io.imem.instruction(63, 32)
  } .otherwise {
    instruction := io.imem.instruction(31, 0)
  }

  
  control.io.opcode := instruction(6, 0)

  
  io.dmem.address := alu.io.result
  io.dmem.maskmode := instruction(13,12)
  io.dmem.memwrite := true.B
  when(instruction(14)===1.U){io.dmem.sext := true.B}
  .elsewhen(instruction(14)=== 0.U){io.dmem.sext := false.B}

  when(control.io.memop === 0.U){io.dmem.valid := false.B}
  .elsewhen(control.io.memop === 1.U){io.dmem.valid := true.B}
 io.dmem.writedata := registers.io.readdata2
 

  registers.io.readreg1 := instruction(19, 15)
  registers.io.readreg2 := instruction(24, 20)
  registers.io.writereg := instruction(11, 7) 


  registers.io.wen := (control.io.writeback_src =/= 0.U & registers.io.writereg =/= 0.U)
  when(control.io.writeback_src === 1.U){registers.io.writedata := alu.io.result}
  .elsewhen(control.io.writeback_src === 3.U){registers.io.writedata := io.dmem.readdata}

  
  aluControl.io.aluop := control.io.aluop
  aluControl.io.arth_type := control.io.arth_type
  aluControl.io.int_length := control.io.int_length
  aluControl.io.funct3 := instruction(14, 12)
  aluControl.io.funct7 := instruction(31, 25)
  
   
  jumpDetection.io.funct3 := instruction(14,12)
  jumpDetection.io.operand1 := registers.io.readdata1 
  jumpDetection.io.operand2 := registers.io.readdata2
  jumpDetection.io.jumpop := control.io.jumpop

  jumpPcGen.io.pc := pc
  jumpPcGen.io.offset :=  instruction
  jumpPcGen.io.op1 := registers.io.readdata2
  jumpPcGen.io.pc_plus_offset := jumpDetection.io.pc_plus_offset
  jumpPcGen.io.op1_plus_offset := jumpDetection.io.op1_plus_offset
  

  alu.io.operation := aluControl.io.operation
  alu.io.operand1 := registers.io.readdata1
  immGen.io.instruction := instruction
  when(control.io.op2_src === 0.U){alu.io.operand2 := registers.io.readdata2}
  .elsewhen(control.io.op2_src === 1.U) {alu.io.operand2 :=  immGen.io.sextImm}

 

  pc := Mux( jumpDetection.io.taken ,pc + 4.U, jumpPcGen.io.jumppc)


}

/*
 * Object to make it easier to print information about the CPU
 */
object SingleCycleCPUInfo {
  def getModules(): List[String] = {
    List(
      "dmem",
      "imem",
      "control",
      "registers",
      "csr",
      "aluControl",
      "alu",
      "immGen",
      "jumpDetection",
      "jumpPcGen"
    )
  }
}
