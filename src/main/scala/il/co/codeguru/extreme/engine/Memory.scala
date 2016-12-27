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

import il.co.codeguru.extreme.engine.datatypes._

object RealModeAddress {
  /** Various real-mode memory constants. */
  val NUM_PARAGRAPHS: Int = 64 * 1024
  val PARAGRAPH_SIZE: Int = 0x10
  val PARAGRAPHS_IN_SEGMENT: Int = 0x1000
  val MEMORY_SIZE: Int = NUM_PARAGRAPHS * PARAGRAPH_SIZE
  val MEMORY_FILL_BYTE: Byte = 0xCC.toByte
}

case class RealModeAddress(segment: M86Word, offset: M86Word) {
  val linearAddress: M86Word = segment * RealModeAddress.PARAGRAPH_SIZE + offset

  def addOffset(offsetDelta: Int): RealModeAddress = new RealModeAddress(this.segment, this.offset + offsetDelta)

  override def toString: String = f"[$segment%#04x:$offset%#04x]"
}

trait RealModeMemory {
  def readByte(address: RealModeAddress, execute: Boolean): M86Byte

  def readWord(address: RealModeAddress, execute: Boolean): M86Word

  def writeByte(address: RealModeAddress, value: M86Byte): RealModeMemory

  def writeWord(address: RealModeAddress, value: M86Word): RealModeMemory
}

abstract class AbstractRealModeMemory extends RealModeMemory {
  def readWord(address: RealModeAddress, execute: Boolean): M86Word = {
    // read low byte
    val low = readByte(address, execute)

    // read high byte
    val nextAddress: RealModeAddress = RealModeAddress(address.segment, address.offset + 1)
    val high = readByte(nextAddress, execute)

    M86Word(high, low)
  }

  def writeWord(address: RealModeAddress, value: M86Word): RealModeMemory = {
    val nextAddress: RealModeAddress = RealModeAddress(address.segment, address.offset + 1)
    writeByte(address, value.lowByte).writeByte(nextAddress, value.highByte)
  }
}

trait MemoryAccessListener {
  def readMemory(address: RealModeAddress, value: M86Byte, execute: Boolean)

  def writeMemory(address: RealModeAddress, value: M86Byte)
}

object NullMemoryAccessListener extends MemoryAccessListener {
  override def readMemory(address: RealModeAddress, value: M86Byte, execute: Boolean): Unit = {}

  override def writeMemory(address: RealModeAddress, value: M86Byte): Unit = {}
}

case class RealModeMemoryImpl(memoryBytes: Vector[M86Byte], listener: MemoryAccessListener) extends AbstractRealModeMemory {
  // init memory
  def this(listener: MemoryAccessListener) =
    this(Vector.fill(RealModeAddress.MEMORY_SIZE)(RealModeAddress.MEMORY_FILL_BYTE), listener)

  def readByte(address: RealModeAddress, execute: Boolean): M86Byte = {
    val value: M86Byte = memoryBytes(address.linearAddress)

    listener.readMemory(address, value, execute)

    value
  }

  def writeByte(address: RealModeAddress, value: M86Byte): RealModeMemory = {
    listener.writeMemory(address, value)

    RealModeMemoryImpl(memoryBytes.updated(address.linearAddress, value), this.listener)
  }
}

case class RealModeMemoryRegion(startAddr: RealModeAddress, endAddr: RealModeAddress) {
  def isInRegion(address: RealModeAddress): Boolean =
    between(startAddr.linearAddress, endAddr.linearAddress, address.linearAddress)

  private def between(start: M86Word, end: M86Word, asked: M86Word) = (asked >= start) && (asked <= end)
}

class MemoryException extends Exception

class RestrictedAccessRealModeMemory(memory: RealModeMemory,
                                     readAccessRegions: Array[RealModeMemoryRegion],
                                     writeAccessRegions: Array[RealModeMemoryRegion],
                                     executeAccessRegions: Array[RealModeMemoryRegion])
  extends AbstractRealModeMemory {

  def readByte(address: RealModeAddress, execute: Boolean): M86Byte = {
    // is reading allowed from this address ?
    if (!isAddressInRegions(readAccessRegions, address)) throw new MemoryException
    memory.readByte(address, execute)
  }

  def writeByte(address: RealModeAddress, value: M86Byte): RealModeMemory = {
    // is writing allowed to this address ?
    if (!isAddressInRegions(writeAccessRegions, address)) throw new MemoryException
    memory.writeByte(address, value)
  }

  private def isAddressInRegions(regions: Array[RealModeMemoryRegion], address: RealModeAddress) =
    regions.indexWhere(_.isInRegion(address)) >= 0
}