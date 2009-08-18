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

package com.google.gimd.query

import file.{File, FileType}
import org.junit.Test
import org.junit.Assert._

class MessageQueryTestCase {

  case class Child(name: String, property1: Boolean)

  object ChildType extends UserType[Child] {
    def toMessageBuffer(child: Child) = (new MessageBuffer()) ++
            List(Field("name", child.name), Field("property1", child.property1.toString))
    def toUserObject(m: Message): Child =
      new Child(m.one("name").stringField.value,
                m.one("property1").stringField.value.toBoolean)
  }

  case class SimpleMessage(name: String, children: List[Child])

  object SimpleMessageType extends UserType[SimpleMessage] {
    def toMessageBuffer(sm: SimpleMessage) = (new MessageBuffer()) ++
      List(Field("name", sm.name))
    def toUserObject(m: Message): SimpleMessage = {
      val children = m.all("child").map(_.messageField.value).map(ChildType.toUserObject(_))
      val name = m.one("name").stringField.value
      new SimpleMessage(name, children.toList)
    }
    override def children = Seq(new NestedMember("child", ChildType))
  }

  object MockParentHandler extends Handler

  @Test
  def simpleQuery = {
    import Predicate.functionLiteral2Predicate
    val reader = new java.io.InputStreamReader(
      this.getClass.getResourceAsStream("simpleMessage.gimd")
    )
    val message = gimd.text.Parser.parse(reader)
    val messageHandler = MessageHandler(MockParentHandler, message)

    val predicate = (child: Child) => child.property1 == true
    val queryResult = MessageQuery.simpleQuery(SimpleMessageType, message, predicate,
      MockParentHandler)

    val expectedChildren = List(Child("Child1", true), Child("Child3", true), Child("Child5", true))
    val expectedMessages = expectedChildren.map(ChildType.toMessageBuffer(_).readOnly)
    val handlers = expectedMessages.map(MessageHandler(messageHandler, _))
    val expectedResult = handlers zip expectedChildren

    assertEquals(Set(expectedResult: _*), Set(queryResult: _*))
  }

}
