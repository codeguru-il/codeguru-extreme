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

import il.co.codeguru.extreme.engine.MachineInstructionOpcode.OperationCode
import il.co.codeguru.extreme.engine.datatypes.M86Byte

/**
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class Machine() {
  var activeCpu: Cpu = _
  var memory: RealModeMemory = _

  def boot(memoryBytes: Vector[M86Byte], listener: MemoryAccessListener): Unit = {
    memory = RealModeMemoryImpl(memoryBytes, listener)
  }

  def setActiveCpu(cpu: Cpu): Unit = {
    activeCpu = cpu
  }

  /**
    * Fetch next opcode and execute it
    *
    * @return execution time
    */
  def runNextOpcode(): Int = {
    runOpcode(fetchNextOpcode())
  }

  /**
    * Execute given opcode
    *
    * @return execution time
    */
  def runOpcode(opcode: OperationCode): Int = {
    activeCpu.runOperation(opcode)
  }

  /**
    * @return next operation code, fetched from memory, using active cpu
    */
  def fetchNextOpcode(): OperationCode = {
    val opcodeFetcher: OpcodeFetcher = new OpcodeFetcher(activeCpu)
    val machineInstructionDecoder: MachineInstructionDecoder = new MachineInstructionDecoder(activeCpu, opcodeFetcher)
    machineInstructionDecoder.decode()
  }
}
