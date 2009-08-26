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

import file.{File, FileType}
import org.junit.Test
import org.junit.Assert._
import org.spearce.jgit.lib.ObjectId

final class JGitProviderTestCase extends AbstractJGitTestCase {

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
    val pathPrefix = Some("sm")
    val pathSuffix = Some(".sm")
    val userType = SimpleMessageType
  }

  @Test
  def allPaths {
    val first = SimpleMessage("first", 1)
    val second = SimpleMessage("second", 2)

    val files = List(
      ("sm/toBeIgnored", writeTextContent("this is to be ignored")),
      ("sm/first.sm", writeMessage(SimpleMessageType, first)),
      ("sm/second.sm", writeMessage(SimpleMessageType, second))
    )
    commit(files)

    val db = new JGitProvider(repository)

    val foundFiles = db.all(SimpleMessageFileType).toList

    val expected = List("sm/first.sm", "sm/second.sm")
    assertEquals(expected, foundFiles.map(_.path))
  }

  @Test
  def allSimpleMessages {
    val first = SimpleMessage("first", 1)
    val second = SimpleMessage("second", 2)

    val files = List(
      ("sm/toBeIgnored", writeTextContent("this is to be ignored")),
      ("sm/first.sm", writeMessage(SimpleMessageType, first)),
      ("sm/second.sm", writeMessage(SimpleMessageType, second))
    )
    commit(files)

    val db = new JGitProvider(repository)

    val foundFiles = db.all(SimpleMessageFileType).toList

    val expected = List(first, second)
    assertEquals(expected, foundFiles.map(_.userObject))
  }

  @Test
  def allFilterRegularFilesOnly {
    val dc = org.spearce.jgit.dircache.DirCache.newInCore
    val builder = dc.builder
    val entry = new org.spearce.jgit.dircache.DirCacheEntry("sm/exec.sm")
    entry.setFileMode(org.spearce.jgit.lib.FileMode.EXECUTABLE_FILE)
    entry.setObjectId(writeTextContent("text content"))
    builder.add(entry)
    builder.finish()
    val treeId = dc.writeTree(new org.spearce.jgit.lib.ObjectWriter(repository))
    val commitId = createCommit("Test commit", treeId)
    moveHEAD(commitId)

    val db = new JGitProvider(repository)

    val foundFiles = db.all(SimpleMessageFileType).toList
    assertEquals(Nil, foundFiles)
  }

  private def commit(files: List[(String, ObjectId)]) {
    val treeId = addFiles(files)
    val commitId = createCommit("Test", treeId)
    moveHEAD(commitId)
  }

}
