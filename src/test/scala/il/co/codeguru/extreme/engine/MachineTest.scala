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
import org.scalatest.FunSuite

/**
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-26
  */

class MachineTest extends FunSuite {

  test("mov [bx],al") {
    val program = Vector(
      0x88, 0x07         // 00000005  8807              mov [bx],al
    )

    val (machine: Machine, cpu: Cpu) = getMachineWithProgram(program)

    assert(cpu.state.ip == M86Word(0x00000000))
    val opcode = machine.fetchNextOpcode()

    assert(cpu.state.ip == M86Word(0x00000002))
  }

  test("Bomber: simplest survivor") {
    val program = Vector(
      0xB0, 0xCC,         // 00000000  B0CC              mov al,0xcc
      0xBB, 0x00, 0x00,   // 00000002  BB0000            mov bx,0x0
      0x88, 0x07,         // 00000005  8807              mov [bx],al
      0x43,               // 00000007  43                inc bx
      0xEB, 0xFB          // 00000008  EBFB              jmp short 0x5
    )

    val (machine: Machine, cpu: Cpu) = getMachineWithProgram(program)

    assert(cpu.state.ip == M86Word(0x00000000))
    val opcode1 = machine.fetchNextOpcode()
    val value1: Short = 0xCC
    assert(opcode1 == MOV(Reg8Operand(AL), Immed8Operand(M86Byte(value1))))

    assert(cpu.state.ip == M86Word(0x00000002))
    val opcode2 = machine.fetchNextOpcode()
    val value2: Short = 0x00
    assert(opcode2 == MOV(Reg16Operand(BX), Immed16Operand(M86Word(value2))))

    assert(cpu.state.ip == M86Word(0x00000005))
    val opcode3 = machine.fetchNextOpcode()
    // FixMe: assert(opcode3 == MOV(NearLabelOperand(Reg16Operand(BX)), Reg8Operand(AL)))

    assert(cpu.state.ip == M86Word(0x00000007))
    val opcode4 = machine.fetchNextOpcode()
    assert(opcode4 == INC(Reg16Operand(BX)))

    assert(cpu.state.ip == M86Word(0x00000008))
    val opcode5 = machine.fetchNextOpcode()
    val value5: Short = 0x5
    // FixMe: assert(opcode5 == JMP(ShortLabelOperand(byte8Bits(value5))))
  }

  test("Execute and validate state: mov ax,42; mov bx,ax") {
    val program = Vector(0xB8, 0x2A, 0x00, 0x89, 0xC3)
    val (machine: Machine, cpu: Cpu) = getMachineWithProgram(program)

    // initial state
    val state0 = cpu.state
    assert(state0.ip == M86Word(0))
    assert(state0.ax == M86Word(0))
    assert(state0.bx == M86Word(0))

    // mov ax,42
    val opcode1 = machine.fetchNextOpcode()
    assert(opcode1 == MOV(Reg16Operand(AX), Immed16Operand(M86Word(42))))
    val time1 = machine.runOpcode(opcode1)
    assert(time1 == 8)
    val state1 = cpu.state
    assert(state1.ip == M86Word(3))
    assert(state1.ax == M86Word(42))
    assert(state1.bx == M86Word(0))

    // mov bx,ax
    val opcode2 = machine.fetchNextOpcode()
    assert(opcode2 == MOV(Reg16Operand(BX), Reg16Operand(AX)))
    val time2 = machine.runOpcode(opcode2)
    assert(time2 == 8)
    val state2 = cpu.state
    assert(state2.ip == M86Word(5))
    assert(state2.ax == M86Word(42))
    assert(state2.bx == M86Word(42))
  }

  private def getMachineWithProgram(program: Vector[Int]) = {
    val listener = NullMemoryAccessListener
    val machine = new Machine()
    val initialCpuState = new CpuState()
    val cpu = new Cpu(initialCpuState, machine)
    val memoryInit: Vector[M86Byte] = program.map(M86Byte(_))
    machine.boot(memoryInit, listener)
    machine.setActiveCpu(cpu)
    (machine, cpu)
  }
}
