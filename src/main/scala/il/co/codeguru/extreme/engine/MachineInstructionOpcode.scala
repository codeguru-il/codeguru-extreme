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

import il.co.codeguru.extreme.engine.Register._

/**
  * Operation codes (aka Opcodes) with their respective parameters
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

object MachineInstructionOpcode {

  sealed abstract class OperationCode(val description: String, val instructionType: InstructionType.Value)

  abstract class Operand

  abstract class RegisterOrMemoryOperand extends Operand

  abstract class RegisterOperand extends RegisterOrMemoryOperand

  abstract class MemoryOperand extends RegisterOrMemoryOperand

  abstract class ImmediateOperand extends Operand

  abstract class LabelOperand extends Operand

  case class Reg8Operand(register: ByteRegister) extends RegisterOperand

  case class Reg16Operand(register: WordRegister) extends RegisterOperand

  case class SegRegOperand(register: SegmentRegister) extends Operand

  case class Mem8Operand(address: RealModeAddress) extends MemoryOperand

  case class Mem16Operand(address: RealModeAddress) extends MemoryOperand

  case class AccumulatorOperand(register: Register) extends Operand

  case class Immed8Operand(value: Byte8Bits) extends ImmediateOperand

  case class Immed16Operand(value: Word16Bits) extends ImmediateOperand

  case class ShortLabelOperand(offset: Byte8Bits) extends LabelOperand

  case class NearLabelOperand(offset: Operand) extends LabelOperand

  case class FarLabelOperand(offset: Operand, segment: Operand) extends LabelOperand

  case class NearProcOperand(offset: Operand) extends LabelOperand

  case class FarProcOperand(offset: Operand, segment: Operand) extends LabelOperand

  case class StringOperand(offset: PointerAndIndexRegister)

  // ToDo: add remaining

  /** MOV destination, source
    *
    * MOV transfers a byte or a word from the source operand to the destination operand.
    */
  case class MOV(destination: Operand, source: Operand) extends OperationCode("Move", InstructionType.DataTransfer)

  /** PUSH source
    *
    * PUSH decrements SP (the stack pointer) by two and then transfers a word from the source
    * operand to the top of stack now pointed to by SP. PUSH often is used to place parameters on the
    * stack before calling a procedure; more generally, it is the basic means of storing temporary data
    * on the stack.
    */
  case class PUSH(source: Operand) extends OperationCode("Push", InstructionType.DataTransfer)

  /** POP destination
    *
    * POP transfers the word at the current top of stack (pointed to by SP) to the destination operand,
    * and then increments SP by two to point to the new top of stack. POP can be used to move temporary
    * variables from the stack to registers or memory.
    */
  case class POP(destination: Operand) extends OperationCode("Pop", InstructionType.DataTransfer)

  /* General Purpose Data Transfers */

  /** XCHG destination, source
    *
    * XCHG (exchange) switches the contents of the source and destination (byte or word) operands.
    * When used in conjunction with the LOCK prefix, XCHG can test and set a semaphore that controls
    * access to a resource shared by multiple processors (see section 2.5).
    */
  case class XCHG(destination: Operand, source: Operand) extends OperationCode("Exchange", InstructionType.DataTransfer)

  /** XLAT translate-table
    *
    * XLAT (translate) replaces a byte in the AL register with a byte from a 256-byte, user-coded translation table.
    * Register BX is assumed to point to the beginning of the table. The byte in AL is used as an index into the table
    * and is replaced by the byte at the offset in the table corresponding to AL's binary value. The first byte in the
    * table has an offset of O. For example, if AL contains 5H, and the sixth element of the translation table contains
    * 33H, then AL will contain 33H following the instruction. XLAT is useful for translating characters from one code
    * to another, the classic example being ASCII to EBCDIC or the reverse.
    */
  case class XLAT(translateTable: Mem16Operand) extends OperationCode("Translate byte to AL", InstructionType.DataTransfer)

  /** IN accumulator,port
    *
    * IN transfers a byte or a word from an input port to the AL register or the AX register, respectively. The port
    * number may be specified either with an immediate byte constant, allowing access to ports numbered 0 through 255,
    * or with a number previously placed in the DX register, allowing variable access (by changing the value in DX) to
    * ports numbered from 0 through 65,535.
    */
  case class IN(accumulator: AccumulatorOperand, port: Operand) extends OperationCode("Input from", InstructionType.DataTransfer)

  /** OUT port, accumulator
    *
    * OUT transfers a byte or a word from the AL register or the AX register, respectively, to an output port. The port
    * number may be specified either with an immediate byte constant; allowing access to ports numbered 0 through 255,
    * or with a number previously placed in register DX, allowing variable access (by changing the value in DX) to ports
    * numbered from 0 through 65,535.
    */
  case class OUT(accumulator: AccumulatorOperand, port: Operand) extends OperationCode("Output to", InstructionType.DataTransfer)

  /** LEA destination,source
    *
    * LEA (load effective address) transfers the offset of the source operand (rather than its value) to the destination
    * operand. The source operand must be a memory operand, and the destination operand must be a 16-bit general
    * register. LEA does not affect any flags. The XLA T and string instructions assume that certain registers point to
    * operands; LEA can be used to load these registers (e.g., 10'lding BX with the address of the translate table used
    * by the XLA T instruction).
    */
  case class LEA(destination: Operand, source: Operand) extends OperationCode("Load EA to register", InstructionType.DataTransfer)

  /** LOS destination,source
    *
    * LDS (load pointer using DS) transfers a 32-bit pointer variable from the source operand, which must be a memory
    * operand, to the destination operand and register DS. The offset word of the pointer is transferred to the
    * destination operand, which may be any 16-bit general register. The segment word of the pointer is transferred to
    * register DS. Specifying SI as the destination operand is a convenient way to prepare to process a source string
    * that is not in the current data segment (string instructions assume that the source string is located in the
    * current data segment and that SI contains the offset of the string).
    */
  case class LDS(destination: Operand, source: Operand) extends OperationCode("Load pointer to DS", InstructionType.DataTransfer)

  /** LES destination, source
    *
    * LES (load pointer using ES) transfers a 32-bit pointer variable from the source operand, which must be a memory
    * operand, to the destination operand and register ES. The offset word of the pointer is transferred to the
    * destination operand, which may be any 16-bit general register. The segment word of the pointer is transferred to
    * register ES. Specifying DI as the destination operand is a convenient way to prepare to process a destination
    * string that is not in the current extra segment. (The destination string must be located in the extra segment, and
    * DI must contain the offset of the string.)
    */
  case class LES(destination: Operand, source: Operand) extends OperationCode("Load pointer to ES", InstructionType.DataTransfer)

  /** Address Object Transfers
    *
    * These instructions manipulate the addresses of variables rather than the contents or values of variables. They are
    * most useful for list processing, based variables, and string operations.
    */

  /** LAHF
    *
    * LAHF (load register AH from flags) copies SF, ZF, AF, PF and CF (the 8080/8085 flags) into bits 7, 6, 4, 2 and 0,
    * respectively, of register AH (see figure 2-32). The content of bits 5, 3 and 1 is undefined; the flags themselves
    * are not affected. LAHF is provided primarily for converting 8080/8085 assembly language programs to run on an
    * 8086 or 8088.
    */
  case class LAHF() extends OperationCode("Load AH with flags", InstructionType.DataTransfer)

  /**
    * SAHF (store register AH into flags) transfers bits 7,6,4,2 and 0 from register AH into SF, ZF, AF, PF and CF,
    * respectively, replacing whatever values these flags previously had. OF, DF, IF and TF are not affected. This
    * instruction is provided for 8080/8085 compatibility.
    */
  case class SAHF() extends OperationCode("Store AH into flags", InstructionType.DataTransfer)

  /**
    * PUSHF decrements SP (the stack pointer) by two and then transfers all flags to the word at the top of stack
    * pointed to by SP (see figure 2-32). The flags themselves are not affected.
    */
  case class PUSHF() extends OperationCode("Push flags", InstructionType.DataTransfer)

  /** Flag Transfers */

  /**
    * POPF transfers specific bits from the word at the current top of stack (pointed to by register SP) into the
    * 8086/8088 flags, replacing whatever values the flags previously contained (see figure 2-32). SP is then
    * incremented by two to point to the new top of stack. PUSHF and POPF allow a procedure to save and restore a
    * calling program's flags. They also allow a program to change the setting of TF (there is no instruction for
    * updating this flag directly). The change is accomplished by pushing the flags, altering bit 8 of the memory image
    * and then popping the flags.
    */
  case class POPF() extends OperationCode("Pop flags", InstructionType.DataTransfer)

  /**
    * The sum of the two operands, which may be bytes or words, replaces the destination operand. Both operands may be
    * signed or unsigned binary numbers (see AAA and DAA). ADD updates AF, CF, OF, PF, SF and ZF.
    */
  case class ADD(destination: Operand, source: Operand) extends OperationCode("Add", InstructionType.Arithmetic)

  /**
    * ADC (Add with Carry) sums the operands, which may be bytes or words, adds one if CF is set and replaces the
    * destination operand with the result. Both operands may be signed or unsigned binary numbers (see AAA and DAA).
    * ADC updates AF, CF, OF, PF, SF and ZF. Since ADC incorporates a carry from a previous operation, it can be used
    * to write routines to add numbers longer than 16 bits.
    */
  case class ADC(destination: Operand, source: Operand) extends OperationCode("Add with carry", InstructionType.Arithmetic)

  /**
    * INC (Increment) adds one to the destination operand. The operand may be a byte or a word and is treated as an
    * unsigned binary number (see AAA and DAA). INC updates AF, OF, PF, SF and ZF; it does not affect CF.
    */
  case class INC(destination: Operand) extends OperationCode("Increment", InstructionType.Arithmetic)

  /** Arithmetic Instructions **/

  /**
    * AAA (ASCII Adjust for Addition) changes the contents of register AL to a valid unpacked decimal number;
    * the high-order half-byte is zeroed. AAA updates AF and CF; the content of OF, PF, SF and ZF is undefined following
    * execution of AAA.
    */
  case class AAA() extends OperationCode("ASCII adjust for add", InstructionType.Arithmetic)

  /**
    * DAA (Decimal Adjust for Addition) corrects the result of previously adding two valid packed decimal operands
    * (the destination operand must have been register AL). DAA changes the content of AL to a pair of valid packed
    * decimal digits. It updates AF, CF, PF, SF and ZF; the content of OF is undefined following execution of DAA.
    */
  case class DAA() extends OperationCode("Decimal adjust for add", InstructionType.Arithmetic)

  /**
    * The source operand is ~ubtracted from the destination operand, and the result replaces the destination operand.
    * The operands may be bytes or words. Both operands may be signed or unsigned binary numbers (see AAS and DAS).
    * SUB updates AF, CF, OF, PF, SF and ZF.
    */
  case class SUB(destination: Operand, source: Operand) extends OperationCode("Subtract", InstructionType.Arithmetic)

  /**
    * SBB (Subtract with Borrow) subtracts the source from the destination, subtracts one if CF is set, and returns the
    * result to the destination operand. Both operands may be bytes or words. Both operands may be signed or unsigned
    * binary numbers (see AAS and DAS). SBB updates AF, CF, OF, PF, SF and ZF. Since it incorporates a borrow from a
    * previous operation, SBB may be used to write routines that subtract numbers longer than 16 bits.
    */
  case class SBB(destination: Operand, source: Operand) extends OperationCode("Subtract with barrow", InstructionType.Arithmetic)

  /**
    * DEC (Decrement) subtracts one from the destination, which may be a byte or a word. DEC updates AF, OF, PF, SF,
    * and ZF; it does not affect CF.
    */
  case class DEC(destination: Operand) extends OperationCode("Decrement", InstructionType.Arithmetic)

  /**
    * NEG (Negate) subtracts the destination operand, which may be a byte or a word, from 0 and returns the result to
    * the destination. This forms the two's complement of the number, effectively reversing the sign of an integer.
    * If the operand is zero, its sign is not changed. Attempting to negate a byte containing -128 or a word containing
    * -32,768 causes no change to the operand and sets OF. NEG updates AF, CF, OF, PF, SF and ZF. CF is always set
    * except when the operand is zero, in which case it is cleared.
    */
  case class NEG(destination: Operand) extends OperationCode("Negate", InstructionType.Arithmetic)

  /**
    * CMP (Compare) subtracts the source from the destination, which may be bytes or words, but does not return the
    * result. The operands are unchanged, but the flags are updated and can be tested by a subsequent conditional jump
    * instruction. CMP updates AF, CF, OF, PF, SF and ZF. The comparison reflected in the flags is that of the
    * destination to the source. If a CMP instruction is followed by a 1G (jump if greater) instruction, for example,
    * the jump is taken if the destination operand is greater than the source operand.
    */
  case class CMP(destination: Operand, source: Operand) extends OperationCode("Compare", InstructionType.Arithmetic)

  /**
    * AAS (ASCII Adjust for Subtraction) corrects the result of a previous subtraction of two valid unpacked decimal
    * operands (the destination operand must have been specified as register AL). AAS changes the content of AL to a
    * valid unpacked decimal number; the high-order half-byte is zeroed. AAS updates AF and CF; the content of OF, PF,
    * SF and ZF is undefined following execution of AAS.
    */
  case class AAS() extends OperationCode("ASCII adjust for subtract", InstructionType.Arithmetic)

  /**
    * DAS (Decimal Adjust for Subtraction) corrects the result of a previous subtraction of two valid packed decimal
    * operands (the destination operand must have been specified as register AL). DAS changes the content of AL to a
    * pair of valid packed decimal digits. DAS updates AF, CF, PF, SF and ZF; the content of OF is undefined following
    * execution of DAS.
    */
  case class DAS() extends OperationCode("Decimal adjust for subtract", InstructionType.Arithmetic)

  /**
    * MUL (Multiply) performs an unsigned multiplication of the source operand and the accumulator. If the source is a
    * byte, then it is multiplied by register AL, and the double-length result is returned in AH and AL. If the source
    * operand is a word, then it is multiplied by register AX, and the double-length result is returned in registers DX
    * and AX. The operands are treated as unsigned binary numbers (see AAM). If the upper half of the result (AH for
    * byte source, DX for word source) is nonzero, CF and OF are set; otherwise they are cleared. When CF and OF are
    * set, they indicate that AH or DX contains significant digits of the result. The content of AF, PF, SF and ZF is
    * undefined following execution of MUL.
    */
  case class MUL(source: Operand) extends OperationCode("Multiply (unsigned)", InstructionType.Arithmetic)

  /**
    * IMUL (Integer Multiply) performs a signed multiplication of the source operand and the accumulator. If the source
    * is a byte, then it is multiplied by register AL, and the double-length result is returned in AH and AL. If the
    * source is a word, then it is multiplied by register AX, and the double-length result is returned in registers DX
    * and AX. If the upper half of the result (AH for byte source, DX for word source) is not the sign extension of the
    * lower half of the result, CF and OF are set; otherwise they are cleared. When CF and OF are set, they indicate
    * that AH or DX contains significant digits of the result. The content of AF, PF, SF and ZF is undefined following
    * execution of IMUL.
    */
  case class IMUL(source: Operand) extends OperationCode("Integer Multiply", InstructionType.Arithmetic)

  /**
    * AAM (ASCII Adjust for Multiply) corrects the result of a previous multiplication of two valid unpacked decimal
    * operands. A valid 2-digit unpacked decimal number is derived from the content of AH and AL and is returned to AH
    * and AL. The high-order half-bytes of the multiplied operands must have been OH for AAM to produce a correct
    * result. AAM updates PF, SF and ZF; the content of AF, CF and OF is undefined following execution of AAM.
    */
  case class AAM() extends OperationCode("ASCII adjust for multiply", InstructionType.Arithmetic)

  /**
    * DIV (divide) performs an unsigned division of the accumulator (and its extension) by the source operand. If the
    * source operand is a byte, it is divided into the double-length dividend assumed to be in registers AL and AH.
    * The single-length quotient is returned in AL, and the single-length remainder is returned in AH. If the source
    * operand is a word, it is divided into the double-length dividend in registers AX and DX. The single-length
    * quotient is returned in AX, and the single-length remainder is returned in DX. If the quotient exceeds the
    * capacity of its destination register (FFH for byte source, FFFFFH for word source), as when division by zero is
    * attempted, a type 0 interrupt is generated, and the quotient and remainder are undefined. Nonintegral quotients
    * are truncated to integers. The content of AF, CF, OF, PF, SF and ZF is undefined following execution of DIV.
    */
  case class DIV(source: Operand) extends OperationCode("Divide (unsigned)", InstructionType.Arithmetic)

  /**
    * IDIV (Integer Divide) performs a signed division of the accumulator (and its extension) by the source operand.
    * If the source operand is a byte, it is divided into the double-length dividend assumed to be in registers AL and
    * AH; the single-length quotient is returned in AL, and the single-length remainder is returned in AH. For byte
    * integer division, the maximum positive quotient is +127 (7FH) and the minimum negative quotient is -127 (SIH).
    * If the source operand is a word, it is divided into the double-length dividend in registers AX and DX; the
    * single-length quotient is returned in AX, and the single-length remainder is returned in DX. For word integer
    * division, the maximum positive quotient is +32,767 (7FFFH) and the minimum negative quotient is -32,767 (SOOIH).
    * If the quotient is positive and exceeds the maximum, or is negative and is less than the minimum, the quotient and
    * remainder are undefined, and a type 0 interrupt is generated. In particular, this occurs if division by 0 is
    * attempted. Nonintegral quotients are truncated (toward 0) to integers, and the remainder has the same sign as the
    * dividend. The content of AF, CF, OF, PF, SF and ZF is undefined following IDIV.
    */
  case class IDIV(source: Operand) extends OperationCode("Integer divided (signed)", InstructionType.Arithmetic)

  /**
    * AAD (ASCII Adjust for Division) modifies the numerator in AL before dividing two valid unpacked decimal operands
    * so that the quotient produced by the division will be a valid unpacked decimal number. AH must be zero for the
    * subsequent DIV to produce the correct result. The quotient is returned in AL, and the remainder is returned in AH;
    * both high-order half-bytes are zeroed. AAD updates PF, SF and ZF; the content of AF, CF and OF is undefined
    * following execution of AAD.
    */
  case class AAD() extends OperationCode("ASCII adjust for divide", InstructionType.Arithmetic)

  /**
    * CBW (Convert Byte to Word) extends the sign of the byte in register AL throughout register AH. CBW does not affect
    * any flags. CBW can be used to produce a double-length (word) dividend from a byte prior to performing byte
    * division.
    */
  case class CBW() extends OperationCode("Convert byte to word", InstructionType.Arithmetic)

  /**
    * CWD (Convert Word to Doubleword) extends the sign of the word in register AX throughout register DX. CWD does not
    * affect any flags. CWD can be used to produce a double-length (doubleword) dividend from a word prior to performing
    * word division.
    */
  case class CWD() extends OperationCode("Convert byte to word", InstructionType.Arithmetic)

  /**
    * NOT inverts the bits (forms the one's complement) of the byte or word operand.
    */
  case class NOT(destination: Operand) extends OperationCode("Invert", InstructionType.Logic)

  /**
    * AND performs the logical "and" of the two operands (byte or word) and returns the result to the destination
    * operand. A bit in the result is set if both corresponding bits of the original operands are set; otherwise the bit
    * is cleared.
    */
  case class AND(destination: Operand, source: Operand) extends OperationCode("And", InstructionType.Logic)

  /**
    * OR performs the logical "inclusive or" of the two operands (byte or word) and returns the result to the
    * destination operand. A bit in the result is set if either or both corresponding bits in the original operands are
    * set; otherwise the result bit is cleared.
    */
  case class OR(destination: Operand, source: Operand) extends OperationCode("Or", InstructionType.Logic)

  /* Bit Manipulation Instructions */

  /**
    * XOR (Exclusive Or) performs the logical "exclusive or" of the two operands and returns the result to the
    * destination operand. A bit in the result is set if the corresponding bits of the original operands contain
    * opposite values (one is set, the other is cleared); otherwise the result bit is cleared.
    */
  case class XOR(destination: Operand, source: Operand) extends OperationCode("Exclusive or", InstructionType.Logic)

  /**
    * TEST performs the logical "and" of the two operands (byte or word), updates the flags, but does not return the
    * result, i.e., neither operand is changed. If a TEST instruction is followed by a JNZ (jump if not zero)
    * instruction, the jump will be taken if there are any corresponding I-bits in both operands.
    */
  case class TEST(destination: Operand, source: Operand) extends OperationCode("And function to flags no result", InstructionType.Logic)

  /**
    * SHL and SAL (Shift Logical Left and Shift Arithmetic Left) perform the same operation and are physically the same
    * instruction. The destination byte or word is shifted left by the number of bits specified in the count operand.
    * Zeros are shifted in on the right. If the sign bit retains its original value, then OF is cleared.
    */
  case class SHL_SAL(destination: Operand, count: Operand) extends OperationCode("Shift logical/arithmetic left", InstructionType.Logic)

  /**
    * SHR (Shift Logical Right) shifts the bits in the destination operand (byte or word) to the right by the number of
    * bits specified in the count operand. Zeros are shifted in on the left. If the sign bit retains its original value,
    * then OF is cleared.
    */
  case class SHR(destination: Operand, source: Operand) extends OperationCode("Shift logical right", InstructionType.Logic)

  /**
    * SAR (Shift Arithmetic Right) shifts the bits in the destination operand (byte or word) to the right by the number
    * of bits specified in the count operand. Bits equal to the original high-order (sign) bit are shifted in on the
    * left, preserving the sign of the original value. Note that SAR does not produce the same result as the dividend of
    * an "equivalent" IDIV instruction if the destination operand is negative and I-bits are shifted out. For example,
    * shifting -5 right by one bit yields -3, while integer division of -5 by 2 yields -2. The difference in the
    * instructions is that IDIV truncates all numbers toward zero, while SAR truncates positive numbers toward zero and
    * negative numbers toward negative infinity.
    */
  case class SAAR(destination: Operand, count: Operand) extends OperationCode("Shift arithmetic right", InstructionType.Logic)

  /**
    * ROL (Rotate Left) rotates the destination byte or word left by the number of bits specified in the count operand.
    */
  case class ROL(destination: Operand, count: Operand) extends OperationCode("Rotate left", InstructionType.Logic)

  /**
    * ROR (Rotate Right) operates similar to ROL except that the bits in the destination byte or word are rotated right
    * instead of left.
    */
  case class ROR(destination: Operand, count: Operand) extends OperationCode("Rotate right", InstructionType.Logic)

  /**
    * RCL (Rotate through Carry Left) rotates the bits in the byte or word destination operand to the left by the number
    * of bits specified in the count operand. The carry flag (CF) is treated as "part of" the destination operand; that
    * is, its value is rotated into the low-order bit of the destination, and itself is replaced by the high-order bit
    * of the destination.
    */
  case class RCL(destination: Operand, count: Operand) extends OperationCode("Rotate through carry flag left", InstructionType.Logic)

  /**
    * RCR (Rotate through Carry Right) operates exactly like RCL except that the bits are rotated right instead of left.
    */
  case class RCR(destination: Operand, count: Operand) extends OperationCode("Rotate through carry right", InstructionType.Logic)

  case class MOVSB(destination: StringOperand, source: StringOperand) extends OperationCode("Move byte string", InstructionType.StringManipulation)

  case class MOVSW(destination: StringOperand, source: StringOperand) extends OperationCode("Move word string", InstructionType.StringManipulation)

  case class CMPSB(destination: StringOperand, source: StringOperand) extends OperationCode("Compare byte", InstructionType.StringManipulation)

  /**
    * Repeat, Repeat While Equal, Repeat While Zero, Repeat While Not Equal and Repeat While Not Zero are five mnemonics
    * for two forms of the prefix byte that controls repetition of a subsequent string instruction. The different
    * mnemonics are provided to improve program clarity. The repeat prefixes do not affect the flags.
    *
    * REP is used in conjunction with the MOYS (Move String) and STOS (Store String) instructions and is interpreted as
    * "repeat while not end-of-string" (CX not 0). REPE and REPZ operate identically and are physically the same prefix
    * byte as REP. These instructions are used with the CMPS (Compare String) and SCAS (Scan String) instructions and
    * require ZF (posted by these instructions) to be set before initiating the next repetition. REPNE and REPNZ are two
    * mnemonics for the same prefix byte. These instructions function the same as REPE and REPZ except that the zero
    * flag must be cleared or the repetition is terminated. Note that ZF does not need to be initialized before
    * executing the repeated string instruction.
    *
    * Repeated string sequences are interruptable; the processor will recognize the interrupt before processing the next
    * string element. System interrupt processing is not affected in any way. Upon return from the interrupt, the
    * repeated operation is resumed from the point of interruption. Note, however, that execution does not resume
    * properly if a second or third prefix (i.e., segrnent override or LOCK) has been specified in addition to any of
    * the repeat prefixes. The processor "remembers" only one prefix in effect at the time of the interrupt, the prefix
    * that immediately precedes the string instruction. After returning from the interrupt, processing resumes at this
    * point, but any additional prefixes specified are not in effect. If more than one prefix must be used with a string
    * instruction, interrupts may be disabled for the duration of the repeated execution. However, this will not prevent
    * a non-maskable interrupt from being recognized. Also, the time that the system is unable to respond to interrupts
    * may be unacceptable if long strings are being processed.
    */

  /**
    * MOYS (Move String) transfers a byte or a word from the source string (addressed by SI) to the destination string
    * (addressed by DI) and updates SI and DI to point to the next string element. When used in conjunction with REP,
    * MOYS performs a memory-to-memory block transfer.
    */

  case class CMPSW(destination: StringOperand, source: StringOperand) extends OperationCode("Compare word", InstructionType.StringManipulation)

  case class SCASB(destination: StringOperand) extends OperationCode("Scan byte/word", InstructionType.StringManipulation)

  case class SCASW(destination: StringOperand) extends OperationCode("Scan word", InstructionType.StringManipulation)

  case class LODSB(source: StringOperand) extends OperationCode("Load byte to AL/AX", InstructionType.StringManipulation)

  case class LODSW(source: StringOperand) extends OperationCode("Load word to AL/AX", InstructionType.StringManipulation)

  case class STOSB(destination: StringOperand) extends OperationCode("Store byte from AL/AX", InstructionType.StringManipulation)

  case class STOSW(destination: StringOperand) extends OperationCode("Store word from AL/AX", InstructionType.StringManipulation)

  /**
    * CALL activates an out-of-line procedure, saving information on the stack to permit a RET (return) instruction in
    * the procedure to transfer control back to the instruction following the CALL. The assembler generates a different
    * type of CALL instruction depending on whether the programmer has defined the procedure name as NEAR or FAR. For
    * control to return properly, the type of CALL instruction must match the type of RET instruction that exits from
    * the procedure. (The potential for a mismatch exists if the procedure and the CALL are contained in separately
    * assembled programs.) Different forms of the CALL instruction allow the address of the target procedure to be
    * obtained from the instruction itself (direct CALL) or from a memory location or register referenced by the
    * instruction (indirect CALL). In the following descriptions, bear in mind that the processor automatically adjusts
    * IP to point to the next instruction to be executed, before saving it on the stack.
    *
    * For an intrasegment direct CALL, SP (the stack pointer) is decremented by two and IP is pushed onto the stack. The
    * relative displacement (up to ±32k) of the target procedure from the CALL instruction is then added to the
    * instruction pointer. This form of the CALL instruction is "self-relative" and is appropriate for
    * position-independent (dynamically relocatable) routines in which the CALL and its target are in the same segment
    * and are moved together.
    *
    * An intrasegment indirect CALL may be made through memory or through a register. SP is decremented by two and IP is
    * pushed onto the stack. The offset of the target procedure is obtained from the memory word or 16-bit general
    * register referenced in the instruction and replaces IP.
    *
    * For an intersegment direct CALL, SP is decremented by two, and CS is pushed onto the stack. CS is replaced by the
    * segment word contained in the instruction. SP again is decremented by two. IP is pushed onto the stack and is
    * replaced by the offset word contained in the instruction.
    *
    * For an intersegment indirect CALL (which only may be made through memory), SP is decremented by two, and CS is
    * pushed onto the stack. CS is then replaced by the content of the second word oithe doubleword memory pointer
    * referenced by the instruction. SP again is decremented by two, and IP is pushed onto the stack and is replaced by
    * the content of the first word of the doubleword pointer referenced by the instruction.
    */
  case class CALLN(proc: NearProcOperand) extends OperationCode("Call Near", InstructionType.ControlTransfer)

  case class CALLF(proc: FarProcOperand) extends OperationCode("Call Far", InstructionType.ControlTransfer)

  case class RETN(popValue: Option[Immed16Operand]) extends OperationCode("Return from CALLN", InstructionType.ControlTransfer)

  case class RETF(popValue: Option[Immed16Operand]) extends OperationCode("Return from CALLF", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JMP(target: LabelOperand) extends OperationCode("Unconditional Jump", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JE_JZ(shortLabel: ShortLabelOperand) extends OperationCode("Jump on equal/zero", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JL_JNGE(shortLabel: ShortLabelOperand) extends OperationCode("Jump on less/not greater or equal", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JLE_JNG(shortLabel: ShortLabelOperand) extends OperationCode("Jump on less or equal/not greater", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JB_JBAE_JC(shortLabel: ShortLabelOperand) extends OperationCode("Jump on below/not above or equal/carry", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JBE_JNA(shortLabel: ShortLabelOperand) extends OperationCode("Jump on brlow or equal/not above", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JP_JPE(shortLabel: ShortLabelOperand) extends OperationCode("Jump on parity/parity even", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JO(shortLabel: ShortLabelOperand) extends OperationCode("Jump on overflow", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JS(shortLabel: ShortLabelOperand) extends OperationCode("Jump on sign", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNE_JNZ(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not equal/not zero", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNL_JGE(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not less/greater or equal", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNLE_JG(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not less or equal/greater", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNB_JAE_JNC(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not below/above or equal/not carry", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNBE_JA(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not below or equal/above", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNP_JPO(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not parity/parity odd", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNO(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not overflow", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JNS(shortLabel: ShortLabelOperand) extends OperationCode("Jump on not sign", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class LOOP(shortLabel: ShortLabelOperand) extends OperationCode("Loop CX times", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class LOOPZ_LOOPE(shortLabel: ShortLabelOperand) extends OperationCode("Loop while zero/equal", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class LOOPNZ_LOOPNE(shortLabel: ShortLabelOperand) extends OperationCode("Loop while not zero/equal", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class JCXZ(shortLabel: ShortLabelOperand) extends OperationCode("Jump on CX zero", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class INT(interruptType: Byte) extends OperationCode("Interrupt", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class INTO() extends OperationCode("Interrupt on overflow", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class IRET() extends OperationCode("Interrupt return", InstructionType.ControlTransfer)

  /**
    *
    *
    */
  case class CLC() extends OperationCode("Clear carry", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class CMC() extends OperationCode("Complement carry", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class STC() extends OperationCode("Set carry", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class CLD() extends OperationCode("Clear direction", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class STD() extends OperationCode("Set direction", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class CLI() extends OperationCode("Clear interrupt", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class STI() extends OperationCode("Set interrupt", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class HLT() extends OperationCode("Halt", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class WAIT() extends OperationCode("Wait", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class ESC() extends OperationCode("Escape (to external device)", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class LOCK() extends OperationCode("Bus lock prefix", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class SEGMENT() extends OperationCode("Override prefix", InstructionType.ProcessorControl)

  /**
    *
    *
    */
  case class PUSHA() extends OperationCode("Push All", InstructionType.DataTransfer)

  /**
    *
    *
    */
  case class POPA() extends OperationCode("Pop All", InstructionType.DataTransfer)

  /**
    *
    *
    */
  case class SAR() extends OperationCode("", InstructionType.Arithmetic)

  /**
    *
    *
    */
  case class ENTER() extends OperationCode("", InstructionType.Arithmetic)

  /**
    *
    *
    */
  case class LEAVE() extends OperationCode("", InstructionType.Arithmetic)

  object InstructionType extends Enumeration {
    val DataTransfer, Arithmetic, Logic, StringManipulation, ControlTransfer, ProcessorControl, None = Value
  }

  case object NotUsed extends OperationCode("(not used)", InstructionType.None)

  case object InvalidOpcode extends OperationCode("(invalid)", InstructionType.None)

  /**
    * These are alternate mnemonics for the move string instruction. These mnemonics are coded without operands; they
    * explicitly tell the assembler that a byte string (MOYSB) or a word string. (MOYSW) is to be moved (when MOYS is
    * coded, the assembler determines the string type from the attributes of the operands). These mnemonics are useful
    * when the assembler cannot determine the attributes of a string, e.g., a section of code is being moved.
    */
  case object MOVSB {
    def apply(): MOVSB = MOVSB(StringOperand(DI), StringOperand(SI))
  }

  /**
    * These are alternate mnemonics for the move string instruction. These mnemonics are coded without operands; they
    * explicitly tell the assembler that a byte string (MOYSB) or a word string. (MOYSW) is to be moved (when MOYS is
    * coded, the assembler determines the string type from the attributes of the operands). These mnemonics are useful
    * when the assembler cannot determine the attributes of a string, e.g., a section of code is being moved.
    */
  case object MOVSW {
    def apply(): MOVSW = MOVSW(StringOperand(DI), StringOperand(SI))
  }

  /**
    * CMPS(Compare String) subtracts the destination byte or word (addressed by DI) from the source byte or word
    * (addressed by SI). CMPS affects the flags but does not alter either operand, updates SI and DI to point to the
    * next string element and updates AF, CF, OF, PF, SF and ZF to reflect the relationship of the destination element
    * to the source element. For example, if a JG (Jump if Greater) instruction follows CMPS, the jump is taken if the
    * destination element is greater than the source element. If CMPS is prefixed with REPE or REPZ, the operation is
    * interpreted as "compare while not end-of-string (CX not zero) and strings are equal (ZF = 1)." If CMPS is preceded
    * by REPNE or REPNZ, the operation is interpreted as "compare while not end-of-string (CX not zero) and strings are
    * not equal (ZF = 0)." Thus, CMPS can be used to find matching or differing string elements.
    */
  case object CMPSB {
    def apply(): CMPSB = CMPSB(StringOperand(DI), StringOperand(SI))
  }

  case object CMPSW {
    def apply(): CMPSW = CMPSW(StringOperand(DI), StringOperand(SI))
  }

  /**
    * SCAS (Scan String) subtracts the destination string element (byte or word) addressed by DI from the content of AL
    * (byte string) or AX (word string) and updates the flags, but does not alter the destination string or the
    * accumulator. SCAS also updates DI to point to the next string element and AF, CF, OF, PF, SF and ZF to reflect the
    * relationship of the scan value in ALI AX to the string element. If SCAS is prefixed with REPE or REPZ, the
    * operation is interpreted as "scan while not end-of-string (CX not 0) and string-element = scan-value (ZF = 1)."
    * This form may be used to scan for departure from a given value. If SCAS is prefixed with REPNE or REPNZ, the
    * operation is interpreted as "scan while not end-of-string (CX not 0) and string-element is not equal to scan-value
    * (ZF = 0)." This form may be used to locate a value in a string.
    */
  case object SCASB {
    def apply(): SCASB = SCASB(StringOperand(DI))
  }

  case object SCASW {
    def apply(): SCASW = SCASW(StringOperand(DI))
  }

  /**
    * LODS (Load String) transfers the byte or word string element addressed by SI to register AL or AX, and updates SI
    * to point to the next element in the string. This instruction is not ordinarily repeated since the accumulator
    * would be overwritten by each repetition, and only the last element would be retained. However, LODS is very useful
    * in software loops as part of a more complex string function built up from string primitives and other instructions.
    */
  case object LODSB {
    def apply(): LODSB = LODSB(StringOperand(SI))
  }

  /**
    * Opcodes for 80186
    */

  case object LODSW {
    def apply(): LODSW = LODSW(StringOperand(SI))
  }

  /**
    * STOS (Store String) transfers a byte or word from register AL or AX to the string element addressed by DI and
    * updates DI to point to the next location in the string. As a repeated operation, STOS provides a convenient way to
    * initialize a string to a constant value (e.g., to blank out a print line).
    */
  case object STOSB {
    def apply(): STOSB = STOSB(StringOperand(DI))
  }

  case object STOSW {
    def apply(): STOSW = STOSW(StringOperand(DI))
  }

  /**
    *
    *
    */
  case object RETN {
    def apply(popValue: Immed16Operand): RETN = RETN(Some(popValue))

    def apply(): RETN = RETN(None)
  }

  case object RETF {
    def apply(popValue: Immed16Operand): RETF = RETF(Some(popValue))

    def apply(): RETF = RETF(None)
  }

}
