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

import il.co.codeguru.extreme.engine.MachineInstructionOpcode._
import il.co.codeguru.extreme.engine.Register._
import il.co.codeguru.extreme.engine.datatypes.{M86Byte, M86Word}

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
    case AAA() => ???
    case AAD() => ???
    case AAM() => ???
    case AAS() => ???
    case ADC(destination, source) => ???
    case ADD(destination, source) => ???
    case AND(destination, source) => ???
    case CALLF(proc) => ???
    case CALLN(proc) => ???
    case CBW() => ???
    case CLC() => ???
    case CLD() => ???
    case CLI() => ???
    case CMC() => ???
    case CMP(destination, source) => ???
    case CMPSB(destination, source) => ???
    case CMPSW(destination, source) => ???
    case CWD() => ???
    case DAA() => ???
    case DAS() => ???
    case DEC(destination) => ???
    case DIV(source) => ???
    case ENTER() => ???
    case ESC() => ???
    case HLT() => ???
    case IDIV(source) => ???
    case IMUL(source) => ???
    case IN(accumulator, port) => ???
    case INC(destination) => ???
    case INT(interruptType) => ???
    case INTO() => ???
    case IRET() => ???
    case JBE_JNA(shortLabel) => ???
    case JB_JBAE_JC(shortLabel) => ???
    case JCXZ(shortLabel) => ???
    case JE_JZ(shortLabel) => ???
    case JLE_JNG(shortLabel) => ???
    case JL_JNGE(shortLabel) => ???
    case JMP(target) => ???
    case JNBE_JA(shortLabel) => ???
    case JNB_JAE_JNC(shortLabel) => ???
    case JNE_JNZ(shortLabel) => ???
    case JNLE_JG(shortLabel) => ???
    case JNL_JGE(shortLabel) => ???
    case JNO(shortLabel) => ???
    case JNP_JPO(shortLabel) => ???
    case JNS(shortLabel) => ???
    case JO(shortLabel) => ???
    case JP_JPE(shortLabel) => ???
    case JS(shortLabel) => ???
    case LAHF() => ???
    case LDS(destination, source) => ???
    case LEA(destination, source) => ???
    case LEAVE() => ???
    case LES(destination, source) => ???
    case LOCK() => ???
    case LODSB(source) => ???
    case LODSW(source) => ???
    case LOOP(shortLabel) => ???
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
    case NEG(destination) => ???
    case NOT(destination) => ???
    case OR(destination, source) => ???
    case OUT(accumulator, port) => ???
    case POP(destination) => ???
    case POPA() => ???
    case POPF() => ???
    case PUSH(source) => ???
    case PUSHA() => ???
    case PUSHF() => ???
    case RCL(destination, count) => ???
    case RCR(destination, count) => ???
    case RETF(popValue) => ???
    case RETN(popValue) => ???
    case ROL(destination, count) => ???
    case ROR(destination, count) => ???
    case SAAR(destination, count) => ???
    case SAHF() => ???
    case SAR() => ???
    case SBB(destination, source) => ???
    case SCASB(destination) => ???
    case SCASW(destination) => ???
    case SEGMENT() => ???
    case SHL_SAL(destination, count) => ???
    case SHR(destination, source) => ???
    case STC() => ???
    case STD() => ???
    case STI() => ???
    case STOSB(destination) => ???
    case STOSW(destination) => ???
    case SUB(destination, source) => ???
    case TEST(destination, source) => ???
    case WAIT() => ???
    case XCHG(destination, source) => ???
    case XLAT(translateTable) => ???
    case XOR(destination, source) => ???
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
