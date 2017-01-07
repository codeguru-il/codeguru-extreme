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

import il.co.codeguru.extreme.engine.datatypes.M86Byte

/**
  * Cpu state fields - registers and flags
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

abstract class StateField

object Register extends StateField {

  def getReg8(index: M86Byte): ByteRegister = getByteRegisterFromIndex(index)

  private def getByteRegisterFromIndex(index: M86Byte): ByteRegister = index match {
    case M86Byte(0) => AL
    case M86Byte(1) => CL
    case M86Byte(2) => DL
    case M86Byte(3) => BL
    case M86Byte(4) => AH
    case M86Byte(5) => CH
    case M86Byte(6) => DH
    case M86Byte(7) => BH
  }

  def getReg16(index: M86Byte): WordRegister = getWordRegisterFromIndex(index)

  private def getWordRegisterFromIndex(index: M86Byte): WordRegister = index match {
    case M86Byte(0) => AX
    case M86Byte(1) => CX
    case M86Byte(2) => DX
    case M86Byte(3) => BX
    case M86Byte(4) => SP
    case M86Byte(5) => BP
    case M86Byte(6) => SI
    case M86Byte(7) => DI
  }

  def getSeg(index: M86Byte): SegmentRegister = segmentFromIndex(index)

  private def segmentFromIndex(index: M86Byte): SegmentRegister = index match {
    case M86Byte(0) => ES
    case M86Byte(1) => CS
    case M86Byte(2) => SS
    case M86Byte(3) => DS
    case M86Byte(4) => ES
    case M86Byte(5) => CS
    case M86Byte(6) => SS
    case M86Byte(7) => DS
  }

  sealed abstract class Register(val description: String, val biteLength: Byte)

  sealed abstract class ByteRegister(description: String) extends Register(description, 8)

  sealed abstract class WordRegister(description: String) extends Register(description, 16)

  sealed abstract class DataByteRegister(description: String) extends ByteRegister(description)

  sealed abstract class DataWordRegister(description: String) extends WordRegister(description)

  sealed abstract class PointerAndIndexRegister(description: String) extends WordRegister(description)

  sealed abstract class SegmentRegister(description: String) extends WordRegister(description)

  case object AX extends DataWordRegister("Accumulator")

  case object AH extends DataByteRegister("Accumulator High")

  case object AL extends DataByteRegister("Accumulator Low")

  case object BX extends DataWordRegister("Base")

  case object BH extends DataByteRegister("Base High")

  case object BL extends DataByteRegister("Base Low")

  case object CX extends DataWordRegister("Count")

  case object CH extends DataByteRegister("Count High")

  case object CL extends DataByteRegister("Count Low")

  case object DX extends DataWordRegister("Data")

  case object DH extends DataByteRegister("Data High")

  case object DL extends DataByteRegister("Data Low")

  case object SP extends PointerAndIndexRegister("Stack Pointer")

  case object BP extends PointerAndIndexRegister("Base Pointer")

  case object SI extends PointerAndIndexRegister("Source Index")

  case object DI extends PointerAndIndexRegister("Destination Index")

  case object CS extends SegmentRegister("Code Segment")

  case object DS extends SegmentRegister("Data Segment")

  case object SS extends SegmentRegister("Stack Segment")

  case object ES extends SegmentRegister("Extra Segment")

  case object IP extends WordRegister("Instruction Pointer")

  case object Flags extends WordRegister("Flags")

}

object Flag extends StateField {

  sealed abstract class Flag(val name: String)

  /**
    * If CF (the carry flag) is set, there has been a carry out of, or a borrow into, the high-order bit of the result
    * (S- or 16-bit). The flag is used by instructions that add and subtract multibyte numbers. Rotate instructions can
    * also isolate a bit in memory or a register by placing it in the carry flag.
    */
  case object CF extends Flag("Carry")

  /**
    * If PF (the parity flag) is set, the result has even parity, an even number of I-bits. This flag can be used to
    * check for data transmission errors.
    */
  case object PF extends Flag("Parity")

  /**
    * If AF (the auxiliary carry flag) is set, there has been a carry out of the low nibble into the high nibble or a
    * borrow from the high nibble into the low nibble of an S-bit quantity (low-order byte of a 16-bit quantity). This
    * flag is used by decimal arithmetic instructions.
    */
  case object AF extends Flag("Auxiliary Carry")

  /**
    * If ZF (the zero flag) is set, the result of the operation is O.
    */
  case object ZF extends Flag("Zero")

  /**
    * If SF (the sign flag) is set, the high-order bit of the result is a 1. Since negative binary numbers are
    * represented in the SOS6 and SOSS in standard two's complement notation, SF indicates the sign of the result
    * (0 = positive, 1 = negative).
    */
  case object SF extends Flag("Sign")

  /**
    * If OF (the overflow flag) is set, an arithmetic overflow has occurred; that is, a significant digit has been lost
    * because the size of the result exceeded the capacity of its destination location. An Interrupt On Overflow
    * instruction is available that will generate an interrupt in this situation.
    */
  case object OF extends Flag("Overflow")

  /**
    * Setting IF (the interrupt-enable flag) allows the CPU to recognize external (maskable) interrupt requests.
    * Clearing IF disables these interrupts. IF has no affect on either non-maskable external or internally generated
    * interrupts.
    */
  case object IF extends Flag("Interrupt-Enable")

  /**
    * Setting DF (the direction flag) causes string instructions to auto-decrement; that is, to process strings from
    * high addresses to low addresses, or from "right to left." Clearing DF causes string instructions to
    * auto-increment, or to process strings from "left to right."
    */
  case object DF extends Flag("Direction")

  /**
    * Setting TF (the trap flag) puts the processor into single-step mode for debugging. In this mode, the CPU
    * automatically generates an internal .interrupt after each instruction, allowing a program to be inspected as it
    * executes instruction by instruction. Section 2.10 contains an example showing the use of TF in a single-step and
    * breakpoint routine.
    */
  case object TF extends Flag("TF")

}
