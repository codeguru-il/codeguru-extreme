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

import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer


class RealModeAddressTest extends FunSuite {

  // Create memory locations for the tests to use.
  var memoryLocations = new ListBuffer[(Int, Int)]()
  for (segment <- 0x0 to 0x10) {
    for (offset <- 0x0 to 0x10) {
      memoryLocations.+=((segment, offset))
    }
  }

  test("Segment and offset are saved in the address object") {
    for((segment, offset) <- memoryLocations) {
      val addr = new RealModeAddress(segment, offset)

      assert(addr.segment == segment)
      assert(addr.offset == offset)
    }
  }

  test("Linear address from segment and offset is calculated well") {
    for((segment, offset) <- memoryLocations) {
      val linearAddress = segment * 0x10 + offset
      val addr = new RealModeAddress(segment, offset)

      assert(addr.linearAddress == linearAddress)
    }
  }

  test("Same linear address is calculated from different segment and offsets") {
    for((segment, offset) <- memoryLocations) {
      val segment1 = segment + 0x1
      val offset1 = offset
      val segment2 = segment1 - 0x1
      val offset2 = offset1 + 0x10

      val addr1 = new RealModeAddress(segment1, offset1)
      val addr2 = new RealModeAddress(segment2, offset2)
      assert(addr1.linearAddress == addr2.linearAddress)
    }
  }

  test("Offset is cyclic") {
    for((segment, offset) <- memoryLocations) {
      val addr1 = new RealModeAddress(segment, offset + 0xffff)
      val addr2 = new RealModeAddress(segment, offset - 0x1)

      assert(addr1.segment == addr2.segment)
      assert(addr1.offset == addr2.offset)
    }
  }

  test("Segment is cyclic") {
    for((segment, offset) <- memoryLocations) {
      val addr1 = new RealModeAddress(segment + 0xffff, offset)
      val addr2 = new RealModeAddress(segment - 0x1, offset)

      assert(addr1.segment == addr2.segment)
      assert(addr1.offset == addr2.offset)
    }
  }
}

class RealModeMemoryImplTest extends FunSuite {
  // ToDo: add more tests
}

class RealModeMemoryRegionTest extends FunSuite {
  // ToDo: add more tests
}

class RestrictedAccessRealModeMemoryTest extends FunSuite {
  // ToDo: add more tests
}