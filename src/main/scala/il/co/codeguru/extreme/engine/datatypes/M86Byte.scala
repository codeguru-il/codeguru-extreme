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

class M86Byte(val value: Short) extends AnyVal with ScalaNumericAnyConversions {
  def >>(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value >> x)

  def <<(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value << x)

  def +(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt + x)

  def -(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt - x)

  def *(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt * x)

  def /(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt / x)

  def %(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt % x)

  def &(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt & x)

  def |(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt | x)

  def ^(x: Int)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt ^ x)

  def +(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt + x.value)

  def -(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt - x.value)

  def *(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt * x.value)

  def /(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt / x.value)

  def %(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt % x.value)

  def &(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt & x.value)

  def |(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt | x.value)

  def ^(x: M86Byte)(implicit d: DummyImplicit): M86Byte = M86Byte(value.toInt ^ x.value)

  def ==(x: Int)(implicit d: DummyImplicit): Boolean = value == x

  def >(x: Int)(implicit d: DummyImplicit): Boolean = value > x

  def <(x: Int)(implicit d: DummyImplicit): Boolean = value < x

  def >=(x: Int)(implicit d: DummyImplicit): Boolean = value >= x

  def <=(x: Int)(implicit d: DummyImplicit): Boolean = value <= x

  override def isWhole(): Boolean = true

  override def underlying(): Any = value

  override def byteValue(): Byte = value.toByte

  override def shortValue(): Short = value.toShort

  override def intValue(): Int = value.toInt

  override def longValue(): Long = value.toLong

  override def floatValue(): Float = value.toFloat

  override def doubleValue(): Double = value.toDouble
}

object M86Byte {
  def unapply(byte: M86Byte): Option[Short] = Some((0xFF & byte.value).toShort)

  def apply(value: Byte): M86Byte = new M86Byte((0xFF & value).toShort)

  def apply(value: Short): M86Byte = new M86Byte((0xFF & value).toShort)

  def apply(value: Int): M86Byte = new M86Byte((0xFF & value).toShort)

}
