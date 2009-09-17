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

package com.google.gimd.jgit

import file.FileType
import junit.framework.Assert._
import org.junit.Test
import org.spearce.jgit.lib.ObjectId

class JGitFileTestCase extends AbstractJGitTestCase {

  case class SimpleMessage(name: String, value: Int)

  object SimpleMessageType extends UserType[SimpleMessage] {
    def toUserObject(m: Message): SimpleMessage = {
      val name = m.one("name").stringField.value
      val value = m.one("value").intField.value
      SimpleMessage(name, value)
    }
    def fields = List(FieldSpec("name", StringField, _.name), FieldSpec("value", IntField, _.value))
  }

  object SimpleMessageFileType extends FileType[SimpleMessage] {
    val pathPrefix = None
    val pathSuffix = None
    val userType = SimpleMessageType
    def name(m: Message) = m.one("name").stringField.value
  }

  @Test
  def testMessage {
    val simpleMessage = SimpleMessage("Test", 10)
    val expected: Message = SimpleMessageType.toMessageBuffer(simpleMessage).readOnly

    val blobId = writeTextContent(expected.toString)

    val jGitFile = new JGitFile("test", blobId, SimpleMessageFileType, masterBranch)
    assertEquals(expected, jGitFile.message)
  }

  @Test
  def testUserObject {
    val expected = SimpleMessage("Test", 10)
    val message: Message = SimpleMessageType.toMessageBuffer(expected).readOnly

    val blobId = writeTextContent(message.toString)

    val jGitFile = new JGitFile("test", blobId, SimpleMessageFileType, masterBranch)
    assertEquals(expected, jGitFile.userObject)
  }

  @Test{val expected = classOf[JGitDatabaseException]}
  def testMessageOfNonExistingObject {
    val jGitFile = new JGitFile("test", ObjectId.zeroId, SimpleMessageFileType, masterBranch)
    jGitFile.message
  }
}
