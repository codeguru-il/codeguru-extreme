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

/**
  * Decoder for MOD + REG + REG/MEM encoding in Cpu instructions
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class IndirectAddressingDecoder(val cpu: Cpu) {

  private val m_state = cpu.state
  private val m_fetcher: OpcodeFetcher = cpu.opcodeFetcher
  private var regIndex: Byte = 0
  private var regOrMemIndex: Byte = 0
  private var memAddress: Option[RealModeAddress] = None
  private var regOperand: Operand = _
  private var regOrMemOperand: Operand = _
  private var forcedSegReg: Option[SegmentRegister] = None
  private var repeatWhileEqual: Option[Boolean] = None // true = REP/REPE/REPZ; false = REPNE/REPNZ

  // def getMemAddress: Address = m_memAddress

  def reset(): Unit = {
    val modeByte = m_fetcher.nextByte

    val mode: Byte = ((modeByte >> 6) & 0x03).toByte
    regIndex = ((modeByte >> 3) & 0x07).toByte
    regOrMemIndex = (modeByte & 0x07).toByte

    if (mode == 3) {
      memAddress = None
    }
    else {
      memAddress = Some(getModeAddress(mode))
    }
  }

  private def getModeAddress(mode: Byte): RealModeAddress = {
    val displacement: Int = mode match {
      case 0 => 0
      case 1 => m_fetcher.nextByte.toInt
      case 2 => m_fetcher.nextWord
    }

    regOrMemIndex match {
      case 0 => newAddress(DS, (m_state.bx + m_state.si + displacement).toShort)
      case 1 => newAddress(DS, (m_state.bx + m_state.di + displacement).toShort)
      case 2 => newAddress(SS, (m_state.bp + m_state.si + displacement).toShort)
      case 3 => newAddress(SS, (m_state.bp + m_state.di + displacement).toShort)
      case 4 => newAddress(DS, (m_state.si + displacement).toShort)
      case 5 => newAddress(DS, (m_state.di + displacement).toShort)
      case 6 => if (mode == 0) {
        newAddress(DS, m_fetcher.nextWord)
      }
      else {
        newAddress(SS, (m_state.bp + displacement).toShort)
      }
      case 7 => newAddress(DS, (m_state.bx + m_fetcher.nextByte).toShort)
    }
  }

  def newAddress(segIndex: SegmentRegister, offset: Int): RealModeAddress = {
    val segment: Int = m_state.getRegister(if (forcedSegReg.isDefined) {
      forcedSegReg.get
    } else segIndex)
    new RealModeAddress(segment, offset)
  }

  def getRegIndex: Byte = regIndex

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

  /*
  def setReg8(value: Byte) {
    m_regs.setReg8(m_regIndex, value)
  }

  def setMem8(value: Byte) {
    if (m_memAddress != null) m_memory.writeByte(m_memAddress, value)
    else m_regs.setReg8(m_memIndex, value)
  }

  def setReg16(value: Short) {
    m_regs.setReg16(m_regIndex, value)
  }

  def setSeg(value: Short) {
    m_regs.setSeg(m_regIndex, value)
  }

  def setMem16(value: Short) {
    if (m_memAddress != null) m_memory.writeWord(m_memAddress, value)
    else m_regs.setReg16(m_memIndex, value)
  }
  */

  def getSeg: Reg16Operand = Reg16Operand(Register.getSeg(regIndex))

  def getMem16: Mem16Operand = if (isMemDefined) Mem16Operand(memAddress.get) else throw new IllegalArgumentException("Memory Undefined")

  def getMem16Next: Mem16Operand = if (isMemDefined) Mem16Operand(memAddress.get.addOffset(2)) else throw new IllegalArgumentException("Memory Undefined")

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

  def newAddress(segIndex: SegmentRegister, baseReg: Register, indexReg: Register): RealModeAddress = {
    val segment: Int = m_state.getRegister(if (forcedSegReg.isDefined) {
      forcedSegReg.get
    } else segIndex)
    val offsetBase: Int = m_state.getRegister(baseReg)
    val offsetIndex: Int = m_state.getRegister(indexReg)
    new RealModeAddress(segment, offsetBase + offsetIndex)
  }
}
