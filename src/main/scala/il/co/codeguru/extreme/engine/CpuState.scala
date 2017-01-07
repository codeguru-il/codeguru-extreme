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
import il.co.codeguru.extreme.engine.Register._
import il.co.codeguru.extreme.engine.datatypes.{M86Byte, M86Word}

/**
  * Immutable CPU state (registers and flags)
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

case class CpuState(ax: M86Word, bx: M86Word, cx: M86Word, dx: M86Word, ds: M86Word, es: M86Word, si: M86Word,
                    di: M86Word, ss: M86Word, bp: M86Word, sp: M86Word, cs: M86Word, ip: M86Word, flags: M86Word) {

  def this() = this(ax = M86Word(0), bx = M86Word(0), cx = M86Word(0), dx = M86Word(0), ds = M86Word(0),
    es = M86Word(0), si = M86Word(0), di = M86Word(0), ss = M86Word(0), bp = M86Word(0), sp = M86Word(0),
    cs = M86Word(0), ip = M86Word(0), flags = M86Word(0))

  def getRegister8(register: ByteRegister): M86Byte = register match {
    case AH => this.ah
    case AL => this.al
    case BH => this.bh
    case BL => this.bl
    case CH => this.ch
    case CL => this.cl
    case DH => this.dh
    case DL => this.dl
  }

  def getRegister16(register: WordRegister): M86Word = register match {
    case AX => this.ax
    case BX => this.bx
    case CX => this.cx
    case DX => this.dx
    case SP => this.sp
    case BP => this.bp
    case SI => this.si
    case DI => this.di
    case CS => this.cs
    case DS => this.ds
    case SS => this.ss
    case ES => this.es
    case IP => this.ip
    case Flags => this.flags
  }

  def getFlag(flag: Flag): Boolean = flag match {
    case CF => this.carryFlag
    case PF => this.parityFlag
    case AF => this.auxFlag
    case ZF => this.zeroFlag
    case SF => this.signFlag
    case OF => this.overflowFlag
    case IF => this.interruptFlag
    case DF => this.directionFlag
    case TF => this.trapFlag
  }

  def carryFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_CARRY)

  def parityFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_PARITY)

  def auxFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_AUX)

  def zeroFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_ZERO)

  def signFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_SIGN)

  def trapFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_TRAP)

  def interruptFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_INTERRUPT)

  def directionFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_DIRECTION)

  def overflowFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_OVERFLOW)

  private def isFlagSet(flagMask: M86Word): Boolean = (flags & flagMask) == flagMask

  def csip: RealModeAddress = new RealModeAddress(this.cs, this.ip)

  def sssp: RealModeAddress = new RealModeAddress(this.ss, this.sp)

  def setFlag(flag: Flag, newValue: Boolean): CpuState = flag match {
    case CF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_CARRY, newValue))
    case PF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_PARITY, newValue))
    case AF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_AUX, newValue))
    case ZF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_ZERO, newValue))
    case SF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_SIGN, newValue))
    case OF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_OVERFLOW, newValue))
    case IF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_INTERRUPT, newValue))
    case DF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_DIRECTION, newValue))
    case TF => setRegister16(Register.Flags, setFlag(CpuState.FLAGS_MASK_TRAP, newValue))
  }

  def setRegister16(register: WordRegister, newValue: M86Word): CpuState = register match {
    case AX => copy(ax = newValue)
    case BX => copy(bx = newValue)
    case CX => copy(cx = newValue)
    case DX => copy(dx = newValue)
    case SP => copy(sp = newValue)
    case BP => copy(bp = newValue)
    case SI => copy(si = newValue)
    case DI => copy(di = newValue)
    case CS => copy(cs = newValue)
    case DS => copy(ds = newValue)
    case SS => copy(ss = newValue)
    case ES => copy(es = newValue)
    case IP => copy(ip = newValue)
    case Flags => copy(flags = newValue)
  }

  private def setFlag(flagMask: M86Word, newValue: Boolean): M86Word =
    if (newValue) flags | flagMask else flags & (~flagMask)

  def setRegister8(register: ByteRegister, newValue: M86Byte): CpuState = register match {
    case AH => copy(ax = M86Word(newValue, this.al))
    case AL => copy(ax = M86Word(this.ah, newValue))
    case BH => copy(bx = M86Word(newValue, this.bl))
    case BL => copy(bx = M86Word(this.bh, newValue))
    case CH => copy(cx = M86Word(newValue, this.cl))
    case CL => copy(cx = M86Word(this.ch, newValue))
    case DH => copy(dx = M86Word(newValue, this.dl))
    case DL => copy(dx = M86Word(this.dh, newValue))
  }

  def al: M86Byte = ax.lowByte

  def bl: M86Byte = bx.lowByte

  def cl: M86Byte = cx.lowByte

  def dl: M86Byte = dx.lowByte

  def ah: M86Byte = ax.highByte

  def bh: M86Byte = bx.highByte

  def ch: M86Byte = cx.highByte

  def dh: M86Byte = dx.highByte
}

object CpuState {
  private val FLAGS_MASK_CARRY = M86Word(0x0001)
  private val FLAGS_MASK_PARITY = M86Word(0x0004)
  private val FLAGS_MASK_AUX = M86Word(0x0010)
  private val FLAGS_MASK_ZERO = M86Word(0x0040)
  private val FLAGS_MASK_SIGN = M86Word(0x0080)
  private val FLAGS_MASK_TRAP = M86Word(0x0100)
  private val FLAGS_MASK_INTERRUPT = M86Word(0x0200)
  private val FLAGS_MASK_DIRECTION = M86Word(0x0400)
  private val FLAGS_MASK_OVERFLOW = M86Word(0x0800)
}