// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for arithmetic instruction types (R-type or I-type)
 *                          . 2 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  arth_type    The type of instruction (0 for R-type, 1 for I-type)
 * Input:  int_length   The integer length (0 for 64-bit, 1 for 32-bit)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop       = Input(UInt(2.W))
    val arth_type   = Input(UInt(1.W))
    val int_length    = Input(UInt(1.W))
    val funct7      = Input(UInt(7.W))
    val funct3      = Input(UInt(3.W))

    val operation   = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

    when(io.aluop === 0.U){ io.operation:="b11111".U}
      .elsewhen(io.aluop === 1.U){

         when(io.arth_type === 0.U){
            when((io.int_length === 0.U) & (io.funct7 === "b0000000".U)&(io.funct3 === "b000".U)){io.operation := "b00000".U}//add
            .elsewhen( (io.int_length === 0.U)&(io.funct7 === "b0000000".U)&(io.funct3 === "b001".U)){io.operation :="b01010".U }//sll
            .elsewhen((io.funct7 === "b0000000".U)&(io.funct3 === "b010".U)){io.operation := "b01100".U}//slt
            .elsewhen((io.funct7 === "b0000000".U)&(io.funct3 === "b011".U)){io.operation := "b01111".U}//sltu
            .elsewhen((io.funct7 === "b0000000".U)&(io.funct3 === "b100".U)){io.operation := "b01000".U}//xor
            .elsewhen( (io.int_length === 0.U)&(io.funct7 === "b0000000".U)&(io.funct3 === "b101".U)){io.operation :="b01011".U }//srl
            .elsewhen((io.funct7 === "b0000000".U)&(io.funct3 === "b110".U)){io.operation := "b00111".U}//or
            .elsewhen( (io.funct7 === "b0000000".U)&(io.funct3 === "b111".U)){io.operation := "b00101".U}//and
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000000".U)&(io.funct3 === "b000".U)){io.operation := "b10000".U}//addw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000000".U)&(io.funct3 === "b001".U)){io.operation := "b11010".U}//sllw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000000".U)&(io.funct3 === "b101".U)){io.operation := "b11011".U}//srlw
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0100000".U)&(io.funct3 === "b000".U)){io.operation := "b00001".U}//sub
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0100000".U)&(io.funct3 === "b101".U)){io.operation := "b01001".U}//sra
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0100000".U)&(io.funct3 === "b000".U)){io.operation := "b10001".U}//subw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0100000".U)&(io.funct3 === "b101".U)){io.operation := "b11001".U}//sraw
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b000".U)){io.operation := "b00010".U}//mul
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b001".U)){io.operation := "b10101".U}//mulh
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b010".U)){io.operation := "b11000".U}//mulhsu
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b011".U)){io.operation :="b10111".U }//mulhu
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b100".U)){io.operation := "b00011".U}//div
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b101".U)){io.operation := "b01101".U}//divu
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b110".U)){io.operation := "b00100".U}//rem
            .elsewhen((io.int_length === 0.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b111".U)){io.operation := "b01110".U}//remu
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b000".U)){io.operation := "b10010".U}//mulw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b100".U)){io.operation := "b10011".U}//divw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b101".U)){io.operation := "b11101".U}//divuw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b110".U)){io.operation :="b10100".U }//remw
            .elsewhen((io.int_length === 1.U) & (io.funct7 === "b0000001".U)&(io.funct3 === "b111".U)){io.operation :="b11110".U }//remuw
            
         
        }.elsewhen(io.arth_type === 1.U){

           when((io.int_length === 0.U) & (io.funct3 === "b000".U)){io.operation :="b00000".U} //ADDI
           .elsewhen((io.int_length === 1.U) & (io.funct3 === "b000".U)){io.operation := "b10000".U} //ADDIW
           .elsewhen((io.funct3 === "b010".U)){io.operation := "b01100".U}//SLTI
           .elsewhen((io.funct3 === "b011".U)){io.operation := "b01111".U} // SLTIU
           .elsewhen(io.funct3 === "b111".U){io.operation := "b00101".U}//ANDI
           .elsewhen(io.funct3 === "b110".U){io.operation := "b00111".U}//ORI
           .elsewhen(io.funct3=== "b100".U){io.operation := "b01000".U}//XORI
           .elsewhen((io.int_length === 0.U) & (io.funct7(6,1) === "b000000".U) & (io.funct3 === "b001".U)){io.operation :="b01010".U}//SLLI
           .elsewhen((io.int_length === 1.U) & (io.funct7(6,1) === "b000000".U) & (io.funct3 === "b001".U)){io.operation := "b11010".U}//SLLIW
           .elsewhen((io.int_length === 0.U) &(io.funct7(6,1) === "b000000".U) &(io.funct3 === "b101".U)){io.operation :="b01011".U}//SRLI
           .elsewhen((io.int_length === 1.U) &(io.funct7 === "b000000".U) &(io.funct3 === "b101".U)){io.operation := "b11011".U}//SRLIW
           .elsewhen((io.int_length === 0.U) &(io.funct7(6,1) === "b010000".U) & (io.funct3 === "b101".U)){io.operation := "b01001".U}//SRAI
           .elsewhen((io.int_length === 1.U) &(io.funct7 === "b0100000".U) & (io.funct3 === "b101".U)){io.operation := "b11001".U}//SRAIW
           
                
   
        } 
      


   }.elsewhen(io.aluop === 2.U){
      when(io.arth_type === 0.U){
       when((io.funct3 === "b011".U)){io.operation := "b00000".U} //ld
      }
    
    
  }
  
 




}
