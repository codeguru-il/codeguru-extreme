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

/**
  * Immutable CPU state (registers and flags)
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

case class CpuState(ax: Int, bx: Int, cx: Int, dx: Int, ds: Int, es: Int, si: Int, di: Int, ss: Int, bp: Int,
                    sp: Int, cs: Int, ip: Int, flags: Int) {

  def this() = this(ax = 0, bx = 0, cx = 0, dx = 0, ds = 0, es = 0, si = 0, di = 0, ss = 0, bp = 0,
    sp = 0, cs = 0, ip = 0, flags = 0)

  def getRegister(register: Register): Int = register match {
    case AX => this.ax
    case AH => this.ah
    case AL => this.al
    case BX => this.bx
    case BH => this.bh
    case BL => this.bl
    case CX => this.cx
    case CH => this.ch
    case CL => this.cl
    case DX => this.dx
    case DH => this.dh
    case DL => this.dl
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

  def al: Byte = lowByte(ax)

  private def lowByte(w: Int): Byte = w.toByte

  def bl: Byte = lowByte(bx)

  def cl: Byte = lowByte(cx)

  def dl: Byte = lowByte(dx)

  def ah: Byte = highByte(ax)

  def bh: Byte = highByte(bx)

  def ch: Byte = highByte(cx)

  def dh: Byte = highByte(dx)

  private def highByte(w: Int): Byte = (w >> 8).toByte

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

  private def isFlagSet(flagMask: Int): Boolean = (flags & flagMask) == flagMask

  def overflowFlag: Boolean = isFlagSet(CpuState.FLAGS_MASK_OVERFLOW)

  def csip = new RealModeAddress(this.cs, this.ip)

  def sssp = new RealModeAddress(this.ss, this.sp)

  def setFlag(flag: Flag, newValue: Boolean): CpuState = flag match {
    case CF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_CARRY, newValue))
    case PF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_PARITY, newValue))
    case AF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_AUX, newValue))
    case ZF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_ZERO, newValue))
    case SF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_SIGN, newValue))
    case OF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_OVERFLOW, newValue))
    case IF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_INTERRUPT, newValue))
    case DF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_DIRECTION, newValue))
    case TF => setRegister(Register.Flags, setFlag(CpuState.FLAGS_MASK_TRAP, newValue))
  }

  def setRegister(register: Register, newValue: Int): CpuState = register match {
    case AX => copy(ax = newValue)
    case AH => copy(ax = wordFromBytes(newValue.toByte, this.al))
    case AL => copy(ax = wordFromBytes(this.ah, newValue.toByte))
    case BX => copy(bx = newValue)
    case BH => copy(bx = wordFromBytes(newValue.toByte, this.bl))
    case BL => copy(bx = wordFromBytes(this.bh, newValue.toByte))
    case CX => copy(cx = newValue)
    case CH => copy(cx = wordFromBytes(newValue.toByte, this.cl))
    case CL => copy(cx = wordFromBytes(this.ch, newValue.toByte))
    case DX => copy(dx = newValue)
    case DH => copy(dx = wordFromBytes(newValue.toByte, this.dl))
    case DL => copy(dx = wordFromBytes(this.dh, newValue.toByte))
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

  private def wordFromBytes(highByte: Byte, lowByte: Byte): Int = (highByte << 8) + (lowByte & 0xFF)

  private def setFlag(flagMask: Int, newValue: Boolean): Int = if (newValue) flags | flagMask else flags & (~flagMask)
}

object CpuState {
  private val FLAGS_MASK_CARRY: Int = 0x0001
  private val FLAGS_MASK_PARITY: Int = 0x0004
  private val FLAGS_MASK_AUX: Int = 0x0010
  private val FLAGS_MASK_ZERO: Int = 0x0040
  private val FLAGS_MASK_SIGN: Int = 0x0080
  private val FLAGS_MASK_TRAP: Int = 0x0100
  private val FLAGS_MASK_INTERRUPT: Int = 0x0200
  private val FLAGS_MASK_DIRECTION: Int = 0x0400
  private val FLAGS_MASK_OVERFLOW: Int = 0x0800
}