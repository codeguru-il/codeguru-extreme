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
import il.co.codeguru.extreme.engine.datatypes.M86Byte

/**
  * Decoder for MOD + REG + REG/MEM encoding in Cpu instructions
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class IndirectAddressingDecoder(val opcodeFetcher: OpcodeFetcher) {

  private var regIndex: M86Byte = M86Byte(0)
  private var regOrMemIndex: M86Byte = M86Byte(0)
  private var memAddress: Option[MemoryAddressing] = None
  private var regOperand: Operand = _
  private var regOrMemOperand: Operand = _
  private var forcedSegReg: Option[SegmentRegister] = None
  private var repeatWhileEqual: Option[Boolean] = None // true = REP/REPE/REPZ; false = REPNE/REPNZ

  def reset(): Unit = {
    val modeByte = opcodeFetcher.nextByte()

    val mode: M86Byte = (modeByte >> 6) & 0x03
    regIndex = (modeByte >> 3) & 0x07
    regOrMemIndex = modeByte & 0x07

    if (mode == 3) {
      memAddress = None
    }
    else {
      memAddress = Some(getModeAddress(mode))
    }
  }

  private def getModeAddress(mode: M86Byte): MemoryAddressing = {
    mode match {
      case M86Byte(0) => {
        regOrMemIndex match {
          case M86Byte(0) => MemoryBaseIndexAddressing(BX, SI)
          case M86Byte(1) => MemoryBaseIndexAddressing(BX, DI)
          case M86Byte(2) => MemoryBaseIndexAddressing(BP, SI)
          case M86Byte(3) => MemoryBaseIndexAddressing(BP, DI)
          case M86Byte(4) => MemoryIndexAddressing(SI)
          case M86Byte(5) => MemoryIndexAddressing(DI)
          case M86Byte(6) => MemoryDirectAddressing(Immed16Operand(opcodeFetcher.nextWord()))
          case M86Byte(7) => MemoryBaseAddressing(BX)
        }
      }

      case M86Byte(1) => {
        val displacement = Immed8Operand(opcodeFetcher.nextByte())

        regOrMemIndex match {
          case M86Byte(0) => MemoryBaseIndexDisp8Addressing(BX, SI, displacement)
          case M86Byte(1) => MemoryBaseIndexDisp8Addressing(BX, DI, displacement)
          case M86Byte(2) => MemoryBaseIndexDisp8Addressing(BP, SI, displacement)
          case M86Byte(3) => MemoryBaseIndexDisp8Addressing(BP, DI, displacement)
          case M86Byte(4) => MemoryIndexDisp8Addressing(SI, displacement)
          case M86Byte(5) => MemoryIndexDisp8Addressing(DI, displacement)
          case M86Byte(6) => MemoryBaseDisp8Addressing(BP, displacement)
          case M86Byte(7) => MemoryBaseDisp8Addressing(BX, displacement)
        }
      }
      case M86Byte(2) => {
        val displacement = Immed16Operand(opcodeFetcher.nextWord())

        regOrMemIndex match {
          case M86Byte(0) => MemoryBaseIndexDisp16Addressing(BX, SI, displacement)
          case M86Byte(1) => MemoryBaseIndexDisp16Addressing(BX, DI, displacement)
          case M86Byte(2) => MemoryBaseIndexDisp16Addressing(BP, SI, displacement)
          case M86Byte(3) => MemoryBaseIndexDisp16Addressing(BP, DI, displacement)
          case M86Byte(4) => MemoryIndexDisp16Addressing(SI, displacement)
          case M86Byte(5) => MemoryIndexDisp16Addressing(DI, displacement)
          case M86Byte(6) => MemoryBaseDisp16Addressing(BP, displacement)
          case M86Byte(7) => MemoryBaseDisp16Addressing(BX, displacement)
        }
      }
    }
  }

  def getRegIndex: M86Byte = regIndex

  def getReg8: Reg8Operand = Reg8Operand(Register.getReg8(regIndex))

  def getMem8: Option[Mem8Operand] = if (isMemDefined) Some(Mem8Operand(memAddress.get)) else None

  def isMemDefined: Boolean = memAddress.isDefined

  def getRegOrMem8: RegisterOrMemoryOperand = {
    if (isMemDefined) {
      Mem8Operand(memAddress.get)
    }
    else {
      Reg8Operand(Register.getReg8(regOrMemIndex))
    }
  }

  def getReg16: Reg16Operand = Reg16Operand(Register.getReg16(regIndex))

  def getSeg: SegRegOperand = SegRegOperand(Register.getSeg(regIndex))

  def getMem16: Mem16Operand = if (isMemDefined) Mem16Operand(memAddress.get) else throw new IllegalArgumentException("Memory Undefined")

  def getRegOrMem16: RegisterOrMemoryOperand = {
    if (isMemDefined) {
      Mem16Operand(memAddress.get)
    }
    else {
      Reg16Operand(Register.getReg16(regOrMemIndex))
    }
  }

  def forceSegReg(reg: SegmentRegister) {
    forcedSegReg = Option(reg)
  }

  def setRepeatWhileEqual(): Unit = {
    repeatWhileEqual = Some(true)
  }

  def setRepeatWhileNotEqual(): Unit = {
    repeatWhileEqual = Some(false)
  }
}
