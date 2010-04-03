// Copyright (C) 2009 The Android Open Source Project
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

package com.google.gimd

import org.junit.Test
import org.junit.Assert._

class FieldTestCase {
  @Test
  def domainOrder {
    val order = List(
      Field("a", 0),
      Field("a", Timestamp.now),
      Field("a", ""),
      Field("a", Message.empty)
    ).toArray

    var i = 0
    while (i < order.length) {
      val a = order(i)
      i += 1

      var j = 0
      while (j < order.length) {
        val b = order(j)
        j += 1

        if (i < j) {
          assertTrue(a < b)
          assertTrue(a <= b)

          assertFalse(a > b)
          assertFalse(a >= b)

          assertFalse(b < a)
          assertFalse(b <= a)

          assertTrue(b > a)
          assertTrue(b >= a)

          assertFalse(a == b)
          assertFalse(b == a)

          assertTrue((a compare b) < 0)
          assertTrue(a.compareTo(b) < 0)

          assertTrue((b compare a) > 0)
          assertTrue(b.compareTo(a) > 0)

        } else if (i == j) {
          assertFalse(a < b)
          assertTrue(a <= b)

          assertFalse(a > b)
          assertTrue(a >= b)

          assertFalse(b < a)
          assertTrue(b <= a)

          assertFalse(b > a)
          assertTrue(b >= a)

          assertTrue(a == b)
          assertTrue(b == a)

          assertEquals(0, a compare b)
          assertEquals(0, a.compareTo(b))

          assertEquals(0, b compare a)
          assertEquals(0, b.compareTo(a))

        } else {
          assertFalse(a < b)
          assertFalse(a <= b)

          assertTrue(a > b)
          assertTrue(a >= b)

          assertTrue(b < a)
          assertTrue(b <= a)

          assertFalse(b > a)
          assertFalse(b >= a)

          assertFalse(a == b)
          assertFalse(b == a)

          assertTrue((a compare b) > 0)
          assertTrue(a.compareTo(b) > 0)

          assertTrue((b compare a) < 0)
          assertTrue(b.compareTo(a) < 0)
        }
      }
    }
  }
}
