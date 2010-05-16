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
import UserType._

class UserTypeTestCase {

  case class Person(id: Int, name: String)

  class PersonUserType extends UserType[Person] {
    val id = FieldSpecOne("id", IntField, _.id)
    val name = FieldSpecOption("name", StringField, _.name, null)
    def toUserObject(m: Message) = Person(id(m), name(m))
    def fields = id :: name
  }

  class MyChildUserType extends PersonUserType

  abstract class MyWrongUserTypeBase[T] extends UserType[T]
  class MyWrongUserType extends MyWrongUserTypeBase[String] {
    def toUserObject(itr: Message): String = ""
    def fields = Nil
  }

  @Test
  def userTypeClass {
    val ut = new PersonUserType
    assertEquals(classOf[Person], ut.userTypeClass)
  }

  @Test
  def userTypeClassInSubtype {
    val ut = new MyChildUserType
    assertEquals(classOf[Person], ut.userTypeClass)
  }

  @Test(expected = classOf[IllegalStateException])
  def userTypeErased {
    val ut = new MyWrongUserType
    ut.userTypeClass
  }

  @Test
  def optionalValueNotPresentInMessage {
    val msg = Message(Field("id", 2))
    val ut = new PersonUserType
    val person = ut.toUserObject(msg)
    assertEquals(Person(2, null), person)
  }

  @Test
  def defaultForOptionalValueInUserObject {
    val ut = new PersonUserType
    val msg = ut.toMessage(Person(3, null))
    assertEquals(Message(Field("id", 3)), msg)
  }

  @Test
  def nonDefaultForOptionalValueInUserObject {
    val ut = new PersonUserType
    val msg = ut.toMessage(Person(3, "John"))
    assertEquals(Message(Field("id", 3), Field("name", "John")), msg)
  }

}
