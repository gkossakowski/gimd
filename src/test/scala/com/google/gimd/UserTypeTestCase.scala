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

class UserTypeTestCase {

  class MyUserType extends UserType[String] {
    def toMessageBuffer(obj: String): MessageBuffer = new MessageBuffer()
    def toUserObject(itr: Message): String = ""
  }

  class MyChildUserType extends MyUserType

  abstract class MyWrongUserTypeBase[T] extends UserType[T]
  class MyWrongUserType extends MyWrongUserTypeBase[String] {
    def toMessageBuffer(obj: String): MessageBuffer = new MessageBuffer()
    def toUserObject(itr: Message): String = ""
  }

  @Test
  def userTypeClass {
    val ut = new MyUserType
    assertEquals(classOf[String], ut.userTypeClass)
  }

  @Test
  def userTypeClassInSubtype {
    val ut = new MyChildUserType
    assertEquals(classOf[String], ut.userTypeClass)
  }

  @Test{val expected = classOf[IllegalStateException]}
  def userTypeErased {
    val ut = new MyWrongUserType
    ut.userTypeClass
  }

}
