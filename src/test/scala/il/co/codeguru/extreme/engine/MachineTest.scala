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
import org.scalatest.FunSuite

/**
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-26
  */

class MachineTest extends FunSuite {

  test("Execute and validate state: mov ax,42; mov bx,ax") {
    val program = Vector(0xB8, 0x2A, 0x00, 0x89, 0xC3)
    val (machine: Machine, cpu: Cpu) = getMachineWithProgram(program)

    // initial state
    val state0 = cpu.state
    assert(state0.ip == 0)
    assert(state0.ax == 0)
    assert(state0.bx == 0)

    // mov ax,42
    val opcode1 = machine.fetchNextOpcode()
    assert(opcode1 == MOV(Reg16Operand(AX), Immed16Operand(42)))
    val time1 = machine.runOpcode(opcode1)
    assert(time1 == 8)
    val state1 = cpu.state
    assert(state1.ip == 3)
    assert(state1.ax == 42)
    assert(state1.bx == 0)

    // mov bx,ax
    val opcode2 = machine.fetchNextOpcode()
    assert(opcode2 == MOV(Reg16Operand(BX), Reg16Operand(AX)))
    val time2 = machine.runOpcode(opcode2)
    assert(time2 == 8)
    val state2 = cpu.state
    assert(state2.ip == 5)
    assert(state2.ax == 42)
    assert(state2.bx == 42)
  }

  private def getMachineWithProgram(program: Vector[Int]) = {
    val listener = NullMemoryAccessListener
    val machine = new Machine()
    val initialCpuState = new CpuState()
    val cpu = new Cpu(initialCpuState, machine)
    val memoryInit: Vector[Byte] = program.map(_.toByte)
    machine.boot(memoryInit, listener)
    machine.setActiveCpu(cpu)
    (machine, cpu)
  }
}
