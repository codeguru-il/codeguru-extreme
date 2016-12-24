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

object RealModeAddress {
  /** Various real-mode memory constants. */
  val NUM_PARAGRAPHS: Int = 64 * 1024
  val PARAGRAPH_SIZE: Int = 0x10
  val PARAGRAPHS_IN_SEGMENT: Int = 0x1000
  val MEMORY_SIZE: Int = NUM_PARAGRAPHS * PARAGRAPH_SIZE
  val MEMORY_FILL_BYTE: Byte = 0xCC.toByte
}

class RealModeAddress(val linearAddress: Int) {
  /** 16-bit real-mode segment */
  val segment: Int = Unsigned.unsignedShort(linearAddress / RealModeAddress.PARAGRAPH_SIZE)

  /** 16-bit real-mode offset */
  val offset: Int = linearAddress % RealModeAddress.PARAGRAPH_SIZE

  def this(segmentParam: Int, offsetParam: Int) = {
    this(Unsigned.unsignedShort(segmentParam) * RealModeAddress.PARAGRAPH_SIZE + Unsigned.unsignedShort(offsetParam))
  }

  def addOffset(offset: Int): RealModeAddress = new RealModeAddress(this.linearAddress + offset)

  override def toString: String = f"[$segment%#04x:$offset%#04x]~[$linearAddress%d]"
}

trait RealModeMemory {
  def readByte(address: RealModeAddress, execute: Boolean): Byte8Bits

  def readWord(address: RealModeAddress, execute: Boolean): Word16Bits

  def writeByte(address: RealModeAddress, value: Byte8Bits): RealModeMemory

  def writeWord(address: RealModeAddress, value: Word16Bits): RealModeMemory
}

abstract class AbstractRealModeMemory extends RealModeMemory {
  def readWord(address: RealModeAddress, execute: Boolean): Word16Bits = {
    // read low byte
    val low = readByte(address, execute)

    // read high byte
    val nextAddress: RealModeAddress = new RealModeAddress(address.segment, (address.offset + 1).toShort)
    val high = readByte(nextAddress, execute)

    word16Bits(((Unsigned.unsignedByte(high) << 8) | Unsigned.unsignedByte(low)).toShort)
  }

  def writeWord(address: RealModeAddress, value: Word16Bits): RealModeMemory = {
    val low: Byte = value.toByte
    val high: Byte = (value >> 8).toByte
    val nextAddress: RealModeAddress = new RealModeAddress(address.segment, (address.offset + 1).toShort)
    writeByte(address, low).writeByte(nextAddress, high)
  }
}

trait MemoryAccessListener {
  def readMemory(address: RealModeAddress, value: Byte8Bits, execute: Boolean)

  def writeMemory(address: RealModeAddress, value: Byte8Bits)
}

case class RealModeMemoryImpl(memoryBytes: Vector[Byte], listener: MemoryAccessListener) extends AbstractRealModeMemory {
  // init memory
  def this(listener: MemoryAccessListener) = this(Vector.fill(RealModeAddress.MEMORY_SIZE)(RealModeAddress.MEMORY_FILL_BYTE), listener)

  def readByte(address: RealModeAddress, execute: Boolean): Byte8Bits = {
    val value: Byte = memoryBytes(address.linearAddress)

    listener.readMemory(address, value, execute)

    byte8Bits(value)
  }

  def writeByte(address: RealModeAddress, value: Byte8Bits): RealModeMemory = {
    listener.writeMemory(address, value)

    RealModeMemoryImpl(memoryBytes.updated(address.linearAddress, value.toByte), this.listener)
  }
}

case class RealModeMemoryRegion(startAddr: RealModeAddress, endAddr: RealModeAddress) {
  def isInRegion(address: RealModeAddress): Boolean =
    between(startAddr.linearAddress, endAddr.linearAddress, address.linearAddress)

  private def between(start: Int, end: Int, asked: Int) = (asked >= start) && (asked <= end)
}

class MemoryException extends Exception

class RestrictedAccessRealModeMemory(memory: RealModeMemory,
                                     readAccessRegions: Array[RealModeMemoryRegion],
                                     writeAccessRegions: Array[RealModeMemoryRegion],
                                     executeAccessRegions: Array[RealModeMemoryRegion])
  extends AbstractRealModeMemory {

  def readByte(address: RealModeAddress, execute: Boolean): Byte8Bits = {
    // is reading allowed from this address ?
    if (!isAddressInRegions(readAccessRegions, address)) throw new MemoryException
    memory.readByte(address, execute)
  }

  def writeByte(address: RealModeAddress, value: Byte8Bits): RealModeMemory = {
    // is writing allowed to this address ?
    if (!isAddressInRegions(writeAccessRegions, address)) throw new MemoryException
    memory.writeByte(address, value)
  }

  private def isAddressInRegions(regions: Array[RealModeMemoryRegion], address: RealModeAddress) =
    regions.indexWhere(_.isInRegion(address)) >= 0
}