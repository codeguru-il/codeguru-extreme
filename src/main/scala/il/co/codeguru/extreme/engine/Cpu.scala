// Copyright (C) 2014-2017 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package il.co.codeguru.extreme.engine

import il.co.codeguru.extreme.engine.Flag._
import il.co.codeguru.extreme.engine.MachineInstructionOpcode._
import il.co.codeguru.extreme.engine.Register._
import il.co.codeguru.extreme.engine.datatypes._

/**
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class Cpu(var state: CpuState, var machine: Machine) {
  val opcodeFetcher = new OpcodeFetcher(this)

  val opcodeDecoder = new MachineInstructionDecoder(this, opcodeFetcher)

  var cpuTime = 0

  def nextOpcode(): Unit = {
    val opCode = opcodeDecoder.decode()

    val opTime = runOperation(opCode)

    cpuTime += opTime
  }

  def runOperation(opcode: OperationCode): Int = opcode match {
    case AAA() => ???; 4
    case AAD() => ???; 60
    case AAM() => ???; 84
    case AAS() => ???; 4
    case ADC(destination, source) => (destination, source) match {
      case (dest: RegisterOperand, src: RegisterOperand) => ???; 3
      case (dest: RegisterOperand, src: MemoryOperand) => ???; 9 /* + EA */
      case (dest: MemoryOperand, src: RegisterOperand) => ???; 16 /* + EA */
      case (dest: RegisterOperand, src: ImmediateOperand) => ???; 4
      case (dest: MemoryOperand, src: ImmediateOperand) => ???; 17 /* + EA */
      case (dest: AccumulatorOperand, src: ImmediateOperand) => ???; 4
    }
    case ADD(destination, source) => ???
    case AND(destination, source) => ???
    case CALLF(proc) => ???
    case CALLN(proc) => ???
    case CBW() => ???; 2
    case CLC() => state = state.setFlag(CF, false); 2
    case CLD() => state = state.setFlag(DF, false); 2
    case CLI() => state = state.setFlag(IF, false); 2
    case CMC() => state = state.setFlag(CF, !state.getFlag(CF)); 2
    case CMP(destination, source) => ???
    case CMPSB(destination, source) => ???
    case CMPSW(destination, source) => ???
    case CWD() => ???; 5
    case DAA() => ???; 4
    case DAS() => ???; 4
    case DEC(destination) => ???
    case DIV(source) => ???
    case ENTER() => ???
    case ESC() => ???
    case HLT() => ???; 2
    case IDIV(source) => ???
    case IMUL(source) => ???
    case IN(accumulator, port) => ???
    case INC(destination) => ???
    case INT(interruptType) => ???
    case INTO() => if (state.overflowFlag) { ???; 53 } else { 4 }
    case IRET() => ???; 24
    case JBE_JNA(shortLabel) => /* (CF OR ZF)=1 */ doJumpIf(shortLabel, state.carryFlag | state.zeroFlag)
    case JB_JBAE_JC(shortLabel) => /* CF=1 */ doJumpIf(shortLabel, state.carryFlag)
    case JCXZ(shortLabel) => /* CX = 0 */ doJumpIf(shortLabel, state.cx == M86Word(0))
    case JE_JZ(shortLabel) => /* ZF=1 */ doJumpIf(shortLabel, state.zeroFlag)
    case JLE_JNG(shortLabel) => /* ((SF XOR OF) OR ZF)=1 */ doJumpIf(shortLabel, (state.signFlag ^ state.overflowFlag) | state.zeroFlag)
    case JL_JNGE(shortLabel) => /* (SF XOR OF)=1 */ doJumpIf(shortLabel, state.signFlag | state.overflowFlag)
    case JMP(target) => ???
    case JNBE_JA(shortLabel) => /* (CF OR ZF)=O */ doJumpIf(shortLabel, !(state.carryFlag | state.zeroFlag))
    case JNB_JAE_JNC(shortLabel) => /* CF=O */ doJumpIf(shortLabel, !state.carryFlag)
    case JNE_JNZ(shortLabel) => /* ZF=O */ doJumpIf(shortLabel, !state.zeroFlag)
    case JNLE_JG(shortLabel) => /* ((SF XOR OF) OR ZF)=O */ doJumpIf(shortLabel, !((state.signFlag ^ state.overflowFlag) | state.zeroFlag))
    case JNL_JGE(shortLabel) => /* (SF XOR OF)=O */ doJumpIf(shortLabel, !(state.signFlag ^ state.overflowFlag))
    case JNO(shortLabel) => /* OF=O */ doJumpIf(shortLabel, !state.overflowFlag)
    case JNP_JPO(shortLabel) => /* PF=O */ doJumpIf(shortLabel, !state.parityFlag)
    case JNS(shortLabel) => /* SF=O */ doJumpIf(shortLabel, !state.signFlag)
    case JO(shortLabel) => /* OF=1 */ doJumpIf(shortLabel, state.overflowFlag)
    case JP_JPE(shortLabel) => /* PF=1 */ doJumpIf(shortLabel, state.parityFlag)
    case JS(shortLabel) => /* SF=1 */ doJumpIf(shortLabel, state.signFlag)
    case LAHF() => ???; 4
    case LDS(destination, source) => ???
    case LEA(destination, source) => ???
    case LEAVE() => ???
    case LES(destination, source) => ???
    case LOCK() => ???; 2
    case LODSB(source) => ???
    case LODSW(source) => ???
    case LOOP(shortLabel) => {
      decreaseCX()
      if (state.getRegister16(CX) != M86Word(0)) {
        // ToDo: jump correctly
        state.setRegister16(IP, M86Word(shortLabel.offset)); 17
      }
      else {
        5
      }
    }
    case LOOPNZ_LOOPNE(shortLabel) => ???
    case LOOPZ_LOOPE(shortLabel) => ???
    //

    case MOV(destination, source) => (destination, source) match {
      //case (dest: Mem8Operand, src: AccumulatorOperand) => MOV(dest, src, 10)
      //case (dest: AccumulatorOperand, src: Mem8Operand) => MOV(dest, src, 10)
      //case (dest: Reg8Operand, src: Reg8Operand) => MOV(dest, src, 2)
      //case (dest: Reg8Operand, src: Mem8Operand) => MOV(dest, src, 8)
      //case (dest: Mem8Operand, src: Reg8Operand) => MOV(dest, src, 9)
      //case (dest: Reg8Operand, src: ImmediateOperand) => MOV(dest, src, 4)
      //case (dest: Mem8Operand, src: ImmediateOperand) => MOV(dest, src, 4)
      //case (dest: SegRegOperand, src: Reg16Operand) => MOV(dest, src, 2)
      //case (dest: SegRegOperand, src: Mem16Operand) => MOV(dest, src, 8)
      //case (dest: Reg16Operand, src: SegRegOperand) => MOV(dest, src, 2)
      //case (dest: Mem8Operand, src: SegRegOperand) => MOV(dest, src, 9)
      case (dest: Reg16Operand, src: Immed16Operand) => state = state.setRegister16(dest.register, src.value); 8
      case (dest: Reg16Operand, src: Reg16Operand) => state = state.setRegister16(dest.register, state.getRegister16(src.register)); 8
    }

    case MOVSB(destination, source) => ???
    case MOVSW(destination, source) => ???
    case MUL(source) => ???
    case NEG(destination) => destination match {
      case (dest: RegisterOperand) => ???; 3
      case (dest: MemoryOperand) => ???; 16
    }
    case NOT(destination) => destination match {
      case (dest: RegisterOperand) => ???; 3
      case (dest: MemoryOperand) => ???; 16
    }
    case OR(destination, source) => ???
    case OUT(accumulator, port) => ???
    case POP(destination) => ???
    case POPA() => ???
    case POPF() => ???; 8
    case PUSH(source) => ???
    case PUSHA() => ???
    case PUSHF() => ???; 10
    case RCL(destination, count) => ???
    case RCR(destination, count) => ???
    case RETF(popValue) => ???
    case RETN(popValue) => ???
    case ROL(destination, count) => ???
    case ROR(destination, count) => ???
    case SAAR(destination, count) => ???
    case SAHF() => ???; 4
    case SAR() => ???
    case SBB(destination, source) => ???
    case SCASB(destination) => ???
    case SCASW(destination) => ???
    case SEGMENT() => ???
    case SHL_SAL(destination, count) => ???
    case SHR(destination, source) => ???
    case STC() => state = state.setFlag(CF, true); 2
    case STD() => state = state.setFlag(DF, true); 2
    case STI() => state = state.setFlag(IF, true); 2
    case STOSB(destination) => ???
    case STOSW(destination) => ???
    case SUB(destination, source) => ???
    case TEST(destination, source) => ???
    case WAIT() => ???
    case XCHG(destination, source) => ???
    case XLAT(translateTable) => ???; 11
    case XOR(destination, source) => ???
  }

  def doJump(shortLabel: ShortLabelOperand): Unit = {
    // ToDo: offset should be signed
    state = state.setRegister16(IP, state.getRegister16(IP) + shortLabel.offset)
  }

  def doJumpIf(shortLabel: ShortLabelOperand, condition: => Boolean, valueIfTrue: Int = 16, valueIfFalse: Int = 4): Int = {
    if (condition) {
      doJump(shortLabel)
      valueIfTrue
    }
    else {
      valueIfFalse
    }
  }

  def decreaseCX(): Unit = {
    state = state.setRegister16(CX, state.getRegister16(CX) - M86Word(1))
  }
}

class OpcodeFetcher(val cpu: Cpu) {
  def nextByte(): M86Byte = {
    val address = cpu.state.csip
    cpu.state = cpu.state.setRegister16(IP, cpu.state.ip + M86Word(1))
    cpu.machine.memory.readByte(address, execute = true)
  }

  def nextWord(): M86Word = {
    val address = cpu.state.csip
    cpu.state = cpu.state.setRegister16(IP, cpu.state.ip + M86Word(2))
    cpu.machine.memory.readWord(address, execute = true)
  }
}
