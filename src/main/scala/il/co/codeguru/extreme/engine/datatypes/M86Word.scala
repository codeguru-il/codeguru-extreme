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
package il.co.codeguru.extreme.engine.datatypes

import scala.math.ScalaNumericAnyConversions

/**
  *
  * @author romi
  * @since 2016-12-27.
  */

class M86Word(val value: Int) extends AnyVal with ScalaNumericAnyConversions {
  def lowByte: M86Byte = M86Byte(value & 0xFF)

  def highByte: M86Byte = M86Byte((value >> 8) & 0xFF)

  def +(x: M86Byte): M86Word = M86Word(value.toInt + x.value)

  def -(x: M86Byte): M86Word = M86Word(value.toInt - x.value)

  def *(x: M86Byte): M86Word = M86Word(value.toInt * x.value)

  def /(x: M86Byte): M86Word = M86Word(value.toInt / x.value)

  def %(x: M86Byte): M86Word = M86Word(value.toInt % x.value)

  def &(x: M86Byte): M86Word = M86Word(value.toInt & x.value)

  def |(x: M86Byte): M86Word = M86Word(value.toInt | x.value)

  def ^(x: M86Byte): M86Word = M86Word(value.toInt ^ x.value)

  def +(x: M86Word): M86Word = M86Word(value.toInt + x.value)

  def -(x: M86Word): M86Word = M86Word(value.toInt - x.value)

  def *(x: M86Word): M86Word = M86Word(value.toInt * x.value)

  def /(x: M86Word): M86Word = M86Word(value.toInt / x.value)

  def %(x: M86Word): M86Word = M86Word(value.toInt % x.value)

  def &(x: M86Word): M86Word = M86Word(value.toInt & x.value)

  def |(x: M86Word): M86Word = M86Word(value.toInt | x.value)

  def ^(x: M86Word): M86Word = M86Word(value.toInt ^ x.value)

  def ==(x: M86Word): Boolean = value == x.value

  def >(x: M86Word): Boolean = value > x.value

  def <(x: M86Word): Boolean = value < x.value

  def >=(x: M86Word): Boolean = value >= x.value

  def <=(x: M86Word): Boolean = value <= x.value

  def unary_~(): M86Word = M86Word(~value)

  override def isWhole(): Boolean = true

  override def underlying(): Any = value.underlying()

  override def byteValue(): Byte = value.toByte

  override def shortValue(): Short = value.toShort

  override def intValue(): Int = value.toInt

  override def longValue(): Long = value.toLong

  override def floatValue(): Float = value.toFloat

  override def doubleValue(): Double = value.toDouble

  override def toString: String = value.formatted("0x%04x")
}

object M86Word {
  def apply(byte: M86Byte): M86Word = M86Word(byte.value.toInt)

  def apply(highByte: M86Byte, lowByte: M86Byte): M86Word = {
    val highByteValue: Short = (highByte.value << 8).toShort
    val lowByteValue: Short = lowByte.value
    val wordValue: Int = highByteValue + lowByteValue
    apply(wordValue)
  }

  def apply(value: Int): M86Word = new M86Word((0xFFFF & value).toInt)
}
