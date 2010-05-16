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

class MessageTestCase {

  @Test
  def getAll {
    val set = Set(Field("name1", 2), Field("name1", "v1"))
    val name0 = Field("name0", 0)
    val message = Message(List(name0) ++ set ++ List(Field("name2", 3)))
    assertSameElements(Set(name0), message.all("name0"))
    assertSameElements(set, message.all("name1"))
    assertSameElements(Set(), message.all("nonExistingName"))
  }

  @Test(expected = classOf[NoSuchElementException])
  def getOneOptionOutOfMany {
    val message = Message(Field("name", "value"), Field("name", 0))
    message.oneOption("name")
  }

  @Test
  def oneOption {
    val message = Message(Field("name", "value"), Field("anotherName", 1))
    assertEquals(Some(Field("name", "value")), message.oneOption("name"))
    assertEquals(None, message.oneOption("nonExistingName"))
  }

  @Test(expected = classOf[NoSuchElementException])
  def oneNonExisiting {
    val message = Message(Field("name", "value"))
    message.one("nonExistingName")
  }

  @Test
  def oneExisting {
    val message = Message(Field("name", "value"))
    assertEquals(Field("name", "value"), message.one("name"))
  }

  private def assertSameElements[T](a: Iterable[T], b: Iterable[T]) =
    org.junit.Assert.assertEquals(Set(a.toSeq: _*), Set(b.toSeq: _*))

}
