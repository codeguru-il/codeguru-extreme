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

package il.co.codeguru.extreme

package object engine {
  type Byte8Bits = Short
  type Word16Bits = Int

  def byte8Bits(value: Byte): Byte8Bits = value.toShort

  def word16Bits(value: Short): Word16Bits = value.toInt

  object Unsigned {
    def unsignedByte(num: Short): Short = (num.toShort & 0xFF).toShort

    def unsignedShort(num: Short): Int = unsignedShort(num.toInt)

    def unsignedShort(num: Int): Int = num & 0xFFFF

    def unsignedInt(num: Int): Long = num.toLong & 0xFFFFFFFF
  }

}