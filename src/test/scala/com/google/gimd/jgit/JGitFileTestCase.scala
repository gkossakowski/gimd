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

import com.google.gimd._
import com.google.gimd.UserType._
import com.google.gimd.file.FileType
import org.eclipse.jgit.lib.ObjectId
import org.junit.Test
import junit.framework.Assert._
import text.Formatter

class JGitFileTestCase extends AbstractJGitTestCase {

  case class SimpleMessage(name: String, value: Int)

  object SimpleMessageType extends UserType[SimpleMessage] {
    val name = FieldSpecOne("name", StringField, _.name)
    val value = FieldSpecOne("value", IntField, _.value)
    def fields = name :: value :: Nil
    def toUserObject(m: Message) = SimpleMessage(name(m), value(m))
  }

  object SimpleMessageFileType extends FileType[SimpleMessage] {
    val pathPrefix = None
    val pathSuffix = None
    val userType = SimpleMessageType
    def name(m: Message) = userType.name(m).toString
  }

  @Test
  def testMessage {
    val simpleMessage = SimpleMessage("Test", 10)
    val expected: Message = SimpleMessageType.toMessageBuffer(simpleMessage).readOnly

    val blobId = writeTextContent(Formatter.format(expected))

    val jGitFile = new JGitFile("test", blobId, SimpleMessageFileType, masterBranch)
    assertEquals(expected, jGitFile.message)
  }

  @Test
  def testUserObject {
    val expected = SimpleMessage("Test", 10)
    val message: Message = SimpleMessageType.toMessageBuffer(expected).readOnly

    val blobId = writeTextContent(Formatter.format(message))

    val jGitFile = new JGitFile("test", blobId, SimpleMessageFileType, masterBranch)
    assertEquals(expected, jGitFile.userObject)
  }

  @Test(expected = classOf[JGitDatabaseException])
  def testMessageOfNonExistingObject {
    val jGitFile = new JGitFile("test", ObjectId.zeroId, SimpleMessageFileType, masterBranch)
    jGitFile.message
  }
}
