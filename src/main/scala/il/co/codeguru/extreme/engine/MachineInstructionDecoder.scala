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
  * Translate byte stream into next instruction
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class MachineInstructionDecoder(val cpu: Cpu, val opcodeFetcher: OpcodeFetcher) {
  def decode(): MachineInstructionOpcode.OperationCode = {
    decode(new IndirectAddressingDecoder(cpu.opcodeFetcher))
  }

  def decode(indirect: IndirectAddressingDecoder): MachineInstructionOpcode.OperationCode = {
    val opcode: M86Byte = readByte()

    opcode match {
      case M86Byte(0x00) => indirect.reset(); ADD(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x01) => indirect.reset(); ADD(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x02) => indirect.reset(); ADD(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x03) => indirect.reset(); ADD(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x04) => ADD(Reg8Operand(AL), readImmed8())
      case M86Byte(0x05) => ADD(Reg16Operand(AX), readImmed16())
      case M86Byte(0x06) => PUSH(Reg16Operand(ES))
      case M86Byte(0x07) => POP(Reg16Operand(ES))
      case M86Byte(0x08) => indirect.reset(); OR(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x09) => indirect.reset(); OR(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x0A) => indirect.reset(); OR(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x0B) => indirect.reset(); OR(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x0C) => OR(Reg8Operand(AL), readImmed8())
      case M86Byte(0x0D) => OR(Reg16Operand(AX), readImmed16())
      case M86Byte(0x0E) => PUSH(Reg16Operand(CS))
      case M86Byte(0x0F) => NotUsed /* Forbidden: POP(RegisterOperand(CS)) */

      case M86Byte(0x10) => indirect.reset(); ADC(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x11) => indirect.reset(); ADC(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x12) => indirect.reset(); ADC(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x13) => indirect.reset(); ADC(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x14) => ADC(Reg8Operand(AL), readImmed8())
      case M86Byte(0x15) => ADC(Reg16Operand(AX), readImmed16())
      case M86Byte(0x16) => PUSH(Reg16Operand(SS))
      case M86Byte(0x17) => POP(Reg16Operand(SS))
      case M86Byte(0x18) => indirect.reset(); SBB(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x19) => indirect.reset(); SBB(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x1A) => indirect.reset(); SBB(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x1B) => indirect.reset(); SBB(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x1C) => SBB(Reg8Operand(AL), readImmed8())
      case M86Byte(0x1D) => SBB(Reg16Operand(AX), readImmed16())
      case M86Byte(0x1E) => PUSH(Reg16Operand(DS))
      case M86Byte(0x1F) => POP(Reg16Operand(DS))

      case M86Byte(0x20) => indirect.reset(); AND(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x21) => indirect.reset(); AND(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x22) => indirect.reset(); AND(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x23) => indirect.reset(); AND(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x24) => AND(Reg8Operand(AL), readImmed8())
      case M86Byte(0x25) => AND(Reg16Operand(AX), readImmed16())
      case M86Byte(0x26) => indirect.forceSegReg(ES); decode(indirect)
      case M86Byte(0x27) => DAA()
      case M86Byte(0x28) => indirect.reset(); SUB(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x29) => indirect.reset(); SUB(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x2A) => indirect.reset(); SUB(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x2B) => indirect.reset(); SUB(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x2C) => SUB(Reg8Operand(AL), readImmed8())
      case M86Byte(0x2D) => SUB(Reg16Operand(AX), readImmed16())
      case M86Byte(0x2E) => indirect.forceSegReg(CS); decode(indirect)
      case M86Byte(0x2F) => DAS()

      case M86Byte(0x30) => indirect.reset(); XOR(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x31) => indirect.reset(); XOR(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x32) => indirect.reset(); XOR(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x33) => indirect.reset(); XOR(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x34) => XOR(Reg8Operand(AL), readImmed8())
      case M86Byte(0x35) => XOR(Reg16Operand(AX), readImmed16())
      case M86Byte(0x36) => indirect.forceSegReg(SS); decode(indirect)
      case M86Byte(0x37) => AAA()
      case M86Byte(0x38) => indirect.reset(); CMP(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x39) => indirect.reset(); CMP(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x3A) => indirect.reset(); CMP(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x3B) => indirect.reset(); CMP(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x3C) => CMP(Reg8Operand(AL), readImmed8())
      case M86Byte(0x3D) => CMP(Reg16Operand(AX), readImmed16())
      case M86Byte(0x3E) => indirect.forceSegReg(SS); decode(indirect)
      case M86Byte(0x3F) => AAS()

      case M86Byte(0x40) => INC(Reg16Operand(AX))
      case M86Byte(0x41) => INC(Reg16Operand(CX))
      case M86Byte(0x42) => INC(Reg16Operand(DX))
      case M86Byte(0x43) => INC(Reg16Operand(BX))
      case M86Byte(0x44) => INC(Reg16Operand(SP))
      case M86Byte(0x45) => INC(Reg16Operand(BP))
      case M86Byte(0x46) => INC(Reg16Operand(SI))
      case M86Byte(0x47) => INC(Reg16Operand(DI))
      case M86Byte(0x48) => DEC(Reg16Operand(AX))
      case M86Byte(0x49) => DEC(Reg16Operand(CX))
      case M86Byte(0x4A) => DEC(Reg16Operand(DX))
      case M86Byte(0x4B) => DEC(Reg16Operand(BX))
      case M86Byte(0x4C) => DEC(Reg16Operand(SP))
      case M86Byte(0x4D) => DEC(Reg16Operand(BP))
      case M86Byte(0x4E) => DEC(Reg16Operand(SI))
      case M86Byte(0x4F) => DEC(Reg16Operand(DI))

      case M86Byte(0x50) => PUSH(Reg16Operand(AX))
      case M86Byte(0x51) => PUSH(Reg16Operand(CX))
      case M86Byte(0x52) => PUSH(Reg16Operand(DX))
      case M86Byte(0x53) => PUSH(Reg16Operand(BX))
      case M86Byte(0x54) => PUSH(Reg16Operand(SP))
      case M86Byte(0x55) => PUSH(Reg16Operand(BP))
      case M86Byte(0x56) => PUSH(Reg16Operand(SI))
      case M86Byte(0x57) => PUSH(Reg16Operand(DI))
      case M86Byte(0x58) => POP(Reg16Operand(AX))
      case M86Byte(0x59) => POP(Reg16Operand(CX))
      case M86Byte(0x5A) => POP(Reg16Operand(DX))
      case M86Byte(0x5B) => POP(Reg16Operand(BX))
      case M86Byte(0x5C) => POP(Reg16Operand(SP))
      case M86Byte(0x5D) => POP(Reg16Operand(BP))
      case M86Byte(0x5E) => POP(Reg16Operand(SI))
      case M86Byte(0x5F) => POP(Reg16Operand(DI))

      case M86Byte(0x60) => PUSHA() // 80186
      case M86Byte(0x61) => POPA()
      case M86Byte(0x62) => NotUsed
      case M86Byte(0x63) => NotUsed
      case M86Byte(0x64) => NotUsed
      case M86Byte(0x65) => NotUsed
      case M86Byte(0x66) => NotUsed
      case M86Byte(0x67) => NotUsed
      case M86Byte(0x68) => PUSH(readImmed16())
      case M86Byte(0x69) => NotUsed // IMUL reg,reg,word
      case M86Byte(0x6A) => PUSH(readImmed8())
      case M86Byte(0x6B) => NotUsed // IMUL reg,reg,byte
      case M86Byte(0x6C) => NotUsed
      case M86Byte(0x6D) => NotUsed
      case M86Byte(0x6E) => NotUsed
      case M86Byte(0x6F) => NotUsed

      case M86Byte(0x70) => JO(readShortLabel())
      case M86Byte(0x71) => JNO(readShortLabel())
      case M86Byte(0x72) => JB_JBAE_JC(readShortLabel())
      case M86Byte(0x73) => JNB_JAE_JNC(readShortLabel())
      case M86Byte(0x74) => JE_JZ(readShortLabel())
      case M86Byte(0x75) => JNE_JNZ(readShortLabel())
      case M86Byte(0x76) => JBE_JNA(readShortLabel())
      case M86Byte(0x77) => JNBE_JA(readShortLabel())
      case M86Byte(0x78) => JS(readShortLabel())
      case M86Byte(0x79) => JNS(readShortLabel())
      case M86Byte(0x7A) => JP_JPE(readShortLabel())
      case M86Byte(0x7B) => JNP_JPO(readShortLabel())
      case M86Byte(0x7C) => JL_JNGE(readShortLabel())
      case M86Byte(0x7D) => JNL_JGE(readShortLabel())
      case M86Byte(0x7E) => JLE_JNG(readShortLabel())
      case M86Byte(0x7F) => JNLE_JG(readShortLabel())

      case M86Byte(0x80) => indirect.reset()
        val immed8 = readImmed8()
        val regOrMem8 = indirect.getRegOrMem8
        indirect.getRegIndex match {
          case M86Byte(0) => ADD(regOrMem8, immed8)
          case M86Byte(1) => OR(regOrMem8, immed8)
          case M86Byte(2) => ADC(regOrMem8, immed8)
          case M86Byte(3) => SBB(regOrMem8, immed8)
          case M86Byte(4) => AND(regOrMem8, immed8)
          case M86Byte(5) => SUB(regOrMem8, immed8)
          case M86Byte(6) => XOR(regOrMem8, immed8)
          case M86Byte(7) => CMP(regOrMem8, immed8)
        }
      case M86Byte(0x81) => indirect.reset()
        val immed16 = readImmed16()
        val regOrMem16 = indirect.getRegOrMem16
        indirect.getRegIndex match {
          case M86Byte(0) => ADD(regOrMem16, immed16)
          case M86Byte(1) => OR(regOrMem16, immed16)
          case M86Byte(2) => ADC(regOrMem16, immed16)
          case M86Byte(3) => SBB(regOrMem16, immed16)
          case M86Byte(4) => AND(regOrMem16, immed16)
          case M86Byte(5) => SUB(regOrMem16, immed16)
          case M86Byte(6) => XOR(regOrMem16, immed16)
          case M86Byte(7) => CMP(regOrMem16, immed16)
        }

      case M86Byte(0x82) => indirect.reset()
        val immed8 = readImmed8()
        val regOrMem8 = indirect.getRegOrMem8
        indirect.getRegIndex match {
          case M86Byte(0) => ADD(regOrMem8, immed8)
          case M86Byte(1) => NotUsed
          case M86Byte(2) => ADC(regOrMem8, immed8)
          case M86Byte(3) => SBB(regOrMem8, immed8)
          case M86Byte(4) => NotUsed
          case M86Byte(5) => SUB(regOrMem8, immed8)
          case M86Byte(6) => NotUsed
          case M86Byte(7) => CMP(regOrMem8, immed8)
        }

      case M86Byte(0x83) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => ADD(indirect.getRegOrMem16, readImmed8())
          case M86Byte(1) => NotUsed
          case M86Byte(2) => ADC(indirect.getRegOrMem16, readImmed8())
          case M86Byte(3) => SBB(indirect.getRegOrMem16, readImmed8())
          case M86Byte(4) => NotUsed
          case M86Byte(5) => SUB(indirect.getRegOrMem16, readImmed8())
          case M86Byte(6) => NotUsed
          case M86Byte(7) => CMP(indirect.getRegOrMem16, readImmed8())
        }
      case M86Byte(0x84) => indirect.reset(); TEST(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x85) => indirect.reset(); TEST(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x86) => indirect.reset(); XCHG(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x87) => indirect.reset(); XCHG(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x88) => indirect.reset(); MOV(indirect.getRegOrMem8, indirect.getReg8)
      case M86Byte(0x89) => indirect.reset(); MOV(indirect.getRegOrMem16, indirect.getReg16)
      case M86Byte(0x8A) => indirect.reset(); MOV(indirect.getReg8, indirect.getRegOrMem8)
      case M86Byte(0x8B) => indirect.reset(); MOV(indirect.getReg16, indirect.getRegOrMem16)
      case M86Byte(0x8C) => indirect.reset()
        if (indirect.getRegIndex > 7) {
          NotUsed
        }
        else {
          MOV(indirect.getRegOrMem16, indirect.getSeg)
        }
      case M86Byte(0x8D) => indirect.reset()
        indirect.getRegOrMem16 match {
          case mem16: Mem8Operand => LEA(indirect.getReg16, mem16)
          case reg16: Reg8Operand => NotUsed
        }

      case M86Byte(0x8E) => indirect.reset()
        indirect.reset()
        if (indirect.getRegIndex > 7) {
          NotUsed
        }
        else {
          MOV(indirect.getSeg, indirect.getRegOrMem16)
        }
      case M86Byte(0x8F) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => POP(indirect.getRegOrMem16)
          case M86Byte(1) => NotUsed
          case M86Byte(2) => NotUsed
          case M86Byte(3) => NotUsed
          case M86Byte(4) => NotUsed
          case M86Byte(5) => NotUsed
          case M86Byte(6) => NotUsed
          case M86Byte(7) => NotUsed
        }

      case M86Byte(0x90) => XCHG(Reg16Operand(AX), Reg16Operand(AX)) /* aka NOP - No Operation */
      case M86Byte(0x91) => XCHG(Reg16Operand(AX), Reg16Operand(CX))
      case M86Byte(0x92) => XCHG(Reg16Operand(AX), Reg16Operand(DX))
      case M86Byte(0x93) => XCHG(Reg16Operand(AX), Reg16Operand(BX))
      case M86Byte(0x94) => XCHG(Reg16Operand(AX), Reg16Operand(SP))
      case M86Byte(0x95) => XCHG(Reg16Operand(AX), Reg16Operand(BP))
      case M86Byte(0x96) => XCHG(Reg16Operand(AX), Reg16Operand(SI))
      case M86Byte(0x97) => XCHG(Reg16Operand(AX), Reg16Operand(DI))
      case M86Byte(0x98) => CBW()
      case M86Byte(0x99) => CWD()
      case M86Byte(0x9A) => CALLF(FarProcOperand(readImmed16(), readImmed16()))
      case M86Byte(0x9B) => WAIT()
      case M86Byte(0x9C) => PUSHF()
      case M86Byte(0x9D) => POPF()
      case M86Byte(0x9E) => SAHF()
      case M86Byte(0x9F) => LAHF()

      case M86Byte(0xA0) => val addr = readAddress(); MOV(Reg8Operand(AL), Mem8Operand(addr))
      case M86Byte(0xA1) => val addr = readAddress(); MOV(Reg16Operand(AX), Mem16Operand(addr))
      case M86Byte(0xA2) => val addr = readAddress(); MOV(Mem8Operand(addr), Reg8Operand(AL))
      case M86Byte(0xA3) => val addr = readAddress(); MOV(Mem16Operand(addr), Reg16Operand(AX))
      case M86Byte(0xA4) => MOVSB()
      case M86Byte(0xA5) => MOVSW()
      case M86Byte(0xA6) => CMPSB()
      case M86Byte(0xA7) => CMPSW()
      case M86Byte(0xA8) => TEST(Reg8Operand(AL), readImmed8())
      case M86Byte(0xA9) => TEST(Reg16Operand(AX), readImmed16())
      case M86Byte(0xAA) => STOSB()
      case M86Byte(0xAB) => STOSW()
      case M86Byte(0xAC) => LODSB()
      case M86Byte(0xAD) => LODSW()
      case M86Byte(0xAE) => SCASB()
      case M86Byte(0xAF) => SCASW()

      case M86Byte(0xB0) => MOV(Reg8Operand(AL), readImmed8())
      case M86Byte(0xB1) => MOV(Reg8Operand(CL), readImmed8())
      case M86Byte(0xB2) => MOV(Reg8Operand(DL), readImmed8())
      case M86Byte(0xB3) => MOV(Reg8Operand(BL), readImmed8())
      case M86Byte(0xB4) => MOV(Reg8Operand(AH), readImmed8())
      case M86Byte(0xB5) => MOV(Reg8Operand(CH), readImmed8())
      case M86Byte(0xB6) => MOV(Reg8Operand(DH), readImmed8())
      case M86Byte(0xB7) => MOV(Reg8Operand(BH), readImmed8())
      case M86Byte(0xB8) => MOV(Reg16Operand(AX), readImmed16())
      case M86Byte(0xB9) => MOV(Reg16Operand(CX), readImmed16())
      case M86Byte(0xBA) => MOV(Reg16Operand(DX), readImmed16())
      case M86Byte(0xBB) => MOV(Reg16Operand(BX), readImmed16())
      case M86Byte(0xBC) => MOV(Reg16Operand(SP), readImmed16())
      case M86Byte(0xBD) => MOV(Reg16Operand(BP), readImmed16())
      case M86Byte(0xBE) => MOV(Reg16Operand(SI), readImmed16())
      case M86Byte(0xBF) => MOV(Reg16Operand(DI), readImmed16())

      case M86Byte(0xC0) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => ROL(indirect.getRegOrMem8, readImmed8())
          case M86Byte(1) => ROR(indirect.getRegOrMem8, readImmed8())
          case M86Byte(2) => RCL(indirect.getRegOrMem8, readImmed8())
          case M86Byte(3) => RCR(indirect.getRegOrMem8, readImmed8())
          case M86Byte(4) => SHL_SAL(indirect.getRegOrMem8, readImmed8())
          case M86Byte(5) => SHR(indirect.getRegOrMem8, readImmed8())
          case M86Byte(6) => NotUsed
          //case M86Byte(7) => SAR(indirect.getRegOrMem8, readImmed8())
        }
      case M86Byte(0xC1) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => ROL(indirect.getRegOrMem16, readImmed8())
          case M86Byte(1) => ROR(indirect.getRegOrMem16, readImmed8())
          case M86Byte(2) => RCL(indirect.getRegOrMem16, readImmed8())
          case M86Byte(3) => RCR(indirect.getRegOrMem16, readImmed8())
          case M86Byte(4) => SHL_SAL(indirect.getRegOrMem16, readImmed8())
          case M86Byte(5) => SHR(indirect.getRegOrMem16, readImmed8())
          case M86Byte(6) => NotUsed
          //case 7) => SAR(indirect.getRegOrMem16, readImmed8())
        }
      case M86Byte(0xC2) => RETN(readImmed16())
      case M86Byte(0xC3) => RETN()
      case M86Byte(0xC4) => indirect.reset(); if (indirect.isMemDefined) LES(indirect.getReg16, indirect.getMem16) else InvalidOpcode
      case M86Byte(0xC5) => indirect.reset(); if (indirect.isMemDefined) LDS(indirect.getReg16, indirect.getMem16) else InvalidOpcode
      case M86Byte(0xC6) => indirect.reset()
        indirect.getRegIndex match {
          //case 0) => MOV(indirect.getMem8, readImmed8())
          case M86Byte(1) => NotUsed
          case M86Byte(2) => NotUsed
          case M86Byte(3) => NotUsed
          case M86Byte(4) => NotUsed
          case M86Byte(5) => NotUsed
          case M86Byte(6) => NotUsed
          case M86Byte(7) => NotUsed
        }
      case M86Byte(0xC7) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => if (indirect.isMemDefined) MOV(indirect.getMem16, readImmed16()) else InvalidOpcode
          case M86Byte(1) => NotUsed
          case M86Byte(2) => NotUsed
          case M86Byte(3) => NotUsed
          case M86Byte(4) => NotUsed
          case M86Byte(5) => NotUsed
          case M86Byte(6) => NotUsed
          case M86Byte(7) => NotUsed
        }
      case M86Byte(0xC8) => ENTER()
      case M86Byte(0xC9) => LEAVE()
      case M86Byte(0xCA) => RETF(readImmed16())
      case M86Byte(0xCB) => RETF()
      case M86Byte(0xCC) => INT(3)
      //case M86Byte(0xCD) => INT(readImmed8())
      case M86Byte(0xCE) => INTO()
      case M86Byte(0xCF) => IRET()

      case M86Byte(0xD0) => indirect.reset()
        indirect.getRegIndex match {
          //case M86Byte(0) => ROL(indirect.getRegOrMem8, 1)
          //case M86Byte(1) => ROR(indirect.getRegOrMem8, 1)
          //case M86Byte(2) => RCL(indirect.getRegOrMem8, 1)
          //case M86Byte(3) => RCR(indirect.getRegOrMem8, 1)
          //case M86Byte(4) => SHL_SAL(indirect.getRegOrMem8, 1)
          //case M86Byte(5) => SHR(indirect.getRegOrMem8, 1)
          case M86Byte(6) => NotUsed
          //case M86Byte(7) => SAR(indirect.getRegOrMem8, 1)
        }
      case M86Byte(0xD1) => indirect.reset()
        indirect.getRegIndex match {
          //case M86Byte(0) => ROL(indirect.getRegOrMem16, 1)
          //case M86Byte(1) => ROR(indirect.getRegOrMem16, 1)
          //case M86Byte(2) => RCL(indirect.getRegOrMem16, 1)
          //case M86Byte(3) => RCR(indirect.getRegOrMem16, 1)
          //case M86Byte(4) => SHL_SAL(indirect.getRegOrMem16, 1)
          //case M86Byte(5) => SHR(indirect.getRegOrMem16, 1)
          case M86Byte(6) => NotUsed
          //case M86Byte(7) => SAR(indirect.getRegOrMem16, 1)
        }
      case M86Byte(0xD2) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => ROL(indirect.getRegOrMem8, Reg8Operand(CL))
          case M86Byte(1) => ROR(indirect.getRegOrMem8, Reg8Operand(CL))
          case M86Byte(2) => RCL(indirect.getRegOrMem8, Reg8Operand(CL))
          case M86Byte(3) => RCR(indirect.getRegOrMem8, Reg8Operand(CL))
          case M86Byte(4) => SHL_SAL(indirect.getRegOrMem8, Reg8Operand(CL))
          case M86Byte(5) => SHR(indirect.getRegOrMem8, Reg8Operand(CL))
          case M86Byte(6) => NotUsed
          //case M86Byte(7) => SAR(indirect.getRegOrMem8, Reg8Operand(CL))
        }
      case M86Byte(0xD3) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => ROL(indirect.getRegOrMem16, Reg8Operand(CL))
          case M86Byte(1) => ROR(indirect.getRegOrMem16, Reg8Operand(CL))
          case M86Byte(2) => RCL(indirect.getRegOrMem16, Reg8Operand(CL))
          case M86Byte(3) => RCR(indirect.getRegOrMem16, Reg8Operand(CL))
          case M86Byte(4) => SHL_SAL(indirect.getRegOrMem16, Reg8Operand(CL))
          case M86Byte(5) => SHR(indirect.getRegOrMem16, Reg8Operand(CL))
          case M86Byte(6) => NotUsed
          //case M86Byte(7) => SAR(indirect.getRegOrMem16, Reg8Operand(CL))
        }
      case M86Byte(0xD4) => AAM()
      case M86Byte(0xD5) => AAD()
      case M86Byte(0xD6) => NotUsed
      case M86Byte(0xD7) => XLAT(MemoryBaseReg8Addressing(BX, AL))
      case M86Byte(0xD8) => NotUsed // ESC???
      case M86Byte(0xD9) => NotUsed
      case M86Byte(0xDA) => NotUsed
      case M86Byte(0xDB) => NotUsed
      case M86Byte(0xDC) => NotUsed
      case M86Byte(0xDD) => NotUsed
      case M86Byte(0xDE) => NotUsed
      case M86Byte(0xDF) => NotUsed

      case M86Byte(0xE0) => LOOPNZ_LOOPNE(readShortLabel())
      case M86Byte(0xE1) => LOOPZ_LOOPE(readShortLabel())
      case M86Byte(0xE2) => LOOP(readShortLabel())
      case M86Byte(0xE3) => JCXZ(readShortLabel())
      case M86Byte(0xE4) => IN(AccumulatorOperand(AL), readImmed8())
      case M86Byte(0xE5) => IN(AccumulatorOperand(AX), readImmed8())
      case M86Byte(0xE6) => OUT(AccumulatorOperand(AL), readImmed8())
      case M86Byte(0xE7) => OUT(AccumulatorOperand(AX), readImmed8())
      case M86Byte(0xE8) => CALLN(NearProcOperand(readImmed16()))
      case M86Byte(0xE9) => JMP(NearLabelOperand(readImmed16()))
      case M86Byte(0xEA) => JMP(FarLabelOperand(readImmed16(), readImmed16()))
      case M86Byte(0xEB) => JMP(readShortLabel())
      case M86Byte(0xEC) => IN(AccumulatorOperand(AL), Reg16Operand(DX))
      case M86Byte(0xED) => IN(AccumulatorOperand(AX), Reg16Operand(DX))
      case M86Byte(0xEE) => OUT(AccumulatorOperand(AL), Reg16Operand(DX))
      case M86Byte(0xEF) => OUT(AccumulatorOperand(AX), Reg16Operand(DX))

      case M86Byte(0xF0) => NotUsed // LOCK
      case M86Byte(0xF1) => NotUsed
      case M86Byte(0xF2) => indirect.setRepeatWhileNotEqual(); decode(indirect) // REPNE/REPNZ
      case M86Byte(0xF3) => indirect.setRepeatWhileEqual(); decode(indirect) // REP/REPE/REPZ
      case M86Byte(0xF4) => HLT()
      case M86Byte(0xF5) => CMC()
      case M86Byte(0xF6) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => TEST(indirect.getRegOrMem8, readImmed8())
          case M86Byte(1) => NotUsed
          case M86Byte(2) => NOT(indirect.getRegOrMem8)
          case M86Byte(3) => NEG(indirect.getRegOrMem8)
          case M86Byte(4) => MUL(indirect.getRegOrMem8)
          case M86Byte(5) => IMUL(indirect.getRegOrMem8)
          case M86Byte(6) => DIV(indirect.getRegOrMem8)
          case M86Byte(7) => IDIV(indirect.getRegOrMem8)
        }
      case M86Byte(0xF7) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => TEST(indirect.getRegOrMem16, readImmed16())
          case M86Byte(1) => NotUsed
          case M86Byte(2) => NOT(indirect.getRegOrMem16)
          case M86Byte(3) => NEG(indirect.getRegOrMem16)
          case M86Byte(4) => MUL(indirect.getRegOrMem16)
          case M86Byte(5) => IMUL(indirect.getRegOrMem16)
          case M86Byte(6) => DIV(indirect.getRegOrMem16)
          case M86Byte(7) => IDIV(indirect.getRegOrMem16)
        }
      case M86Byte(0xF8) => CLC()
      case M86Byte(0xF9) => STC()
      case M86Byte(0xFA) => CLI()
      case M86Byte(0xFB) => STI()
      case M86Byte(0xFC) => CLD()
      case M86Byte(0xFD) => STD()
      case M86Byte(0xFE) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => INC(indirect.getRegOrMem8)
          case M86Byte(1) => DEC(indirect.getRegOrMem8)
          case M86Byte(2) => NotUsed
          case M86Byte(3) => NotUsed
          case M86Byte(4) => NotUsed
          case M86Byte(5) => NotUsed
          case M86Byte(6) => NotUsed
          case M86Byte(7) => NotUsed
        }
      case M86Byte(0xFF) => indirect.reset()
        indirect.getRegIndex match {
          case M86Byte(0) => if (indirect.isMemDefined) INC(indirect.getMem16) else InvalidOpcode
          case M86Byte(1) => if (indirect.isMemDefined) DEC(indirect.getMem16) else InvalidOpcode
          case M86Byte(2) => CALLN(NearProcOperand(indirect.getRegOrMem16))
          case M86Byte(3) => if (indirect.isMemDefined) {
            val segment = indirect.getMem16
            indirect.reset()
            val offset = indirect.getMem16
            CALLF(FarProcOperand(segment, offset))
          } else {
            InvalidOpcode
          }
          case M86Byte(4) => JMP(NearLabelOperand(indirect.getRegOrMem16))
          //case M86Byte(5) => if (indirect.isMemDefined) JMP(FarLabelOperand(indirect.getMem16)) else InvalidOpcode
          case M86Byte(6) => if (indirect.isMemDefined) PUSH(indirect.getMem16) else InvalidOpcode
          case M86Byte(7) => NotUsed
        }
    }
  }

  private def readShortLabel() = ShortLabelOperand(readByte())

  private def readImmed16() = Immed16Operand(readWord())

  private def readImmed8() = Immed8Operand(readByte())

  private def readAddress() = MemoryDirectAddressing(readImmed16())

  private def readByte(): M86Byte = opcodeFetcher.nextByte()

  private def readWord(): M86Word = opcodeFetcher.nextWord()
}

