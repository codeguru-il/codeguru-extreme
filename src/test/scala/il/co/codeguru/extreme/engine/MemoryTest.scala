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

class RealModeAddressTest extends FunSuite {

  test("Linear address from segment and offset") {
    val seg: Int = 0x12
    val offset: Int = 0x05
    val linearAddress = seg * 0x10 + offset

    val addr = new RealModeAddress(seg, offset)

    assert(addr.segment == seg)
    assert(addr.offset == offset)
    assert(addr.linearAddress == linearAddress)
  }

  test("Segment and offset from linear address") {
    val seg: Int = 0x12
    val offset: Int = 0x05
    val linearAddress = seg * 0x10 + offset

    val addr = new RealModeAddress(linearAddress)
    assert(addr.segment == seg)
    assert(addr.offset == offset)
    assert(addr.linearAddress == linearAddress)
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