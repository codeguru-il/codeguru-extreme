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

/**
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class Machine() {
  val listener: MemoryAccessListener = ???
  val activeCpu: Cpu = ???
  var memory: RealModeMemory = _

  def boot(memoryBytes: Vector[Byte]): Unit = {
    memory = RealModeMemoryImpl(memoryBytes, listener)
  }

  /**
    * @return execution time
    */
  def doNextOpcode(): Int = {
    val opcodeFetcher: OpcodeFetcher = new OpcodeFetcher(activeCpu)
    val machineInstructionDecoder: MachineInstructionDecoder = new MachineInstructionDecoder(activeCpu, opcodeFetcher)

    val opcode = machineInstructionDecoder.decode()
    activeCpu.runOperation(opcode)
  }
}
