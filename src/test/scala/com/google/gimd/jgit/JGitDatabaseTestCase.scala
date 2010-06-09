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
import com.google.gimd.file.{File, FileType}
import com.google.gimd.modification.DatabaseModification
import com.google.gimd.query.Predicate
import org.eclipse.jgit.lib.{Constants, ObjectId}
import com.google.gimd.query.Query._
import org.junit.Test
import org.junit.Assert._

final class JGitDatabaseTestCase extends AbstractJGitTestCase {

  case class SimpleMessage(name: String, value: Int)

  object SimpleMessageType extends UserType[SimpleMessage] {
    val name = FieldSpecOne("name", StringField, _.name)
    val value = FieldSpecOne("value", IntField, _.value)
    def fields = name :: value :: Nil
    def toUserObject(m: Message): SimpleMessage = SimpleMessage(name(m), value(m))
  }

  object SimpleMessageFileType extends FileType[SimpleMessage] {
    val pathPrefix = Some("sm/")
    val pathSuffix = Some(".sm")
    val userType = SimpleMessageType
    def name(m: Message) = userType.name(m)
  }

  val fileTypes = SimpleMessageFileType :: Nil

  private def createDatabase = new JGitDatabase(fileTypes, masterBranch)

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

    val foundFiles = withDb { db =>
      db.latestSnapshot.all(SimpleMessageFileType).toList
    } 

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

    val foundFiles = withDb { db =>
      db.latestSnapshot.all(SimpleMessageFileType).toList
    }

    val expected = List(first, second)
    assertEquals(expected, foundFiles.map(_.userObject))
  }

  @Test
  def allFilterRegularFilesOnly {
    val dc = org.eclipse.jgit.dircache.DirCache.newInCore
    val builder = dc.builder
    val entry = new org.eclipse.jgit.dircache.DirCacheEntry("sm/exec.sm")
    entry.setFileMode(org.eclipse.jgit.lib.FileMode.EXECUTABLE_FILE)
    entry.setObjectId(writeTextContent("text content"))
    builder.add(entry)
    builder.finish()
    val treeId = dc.writeTree(new org.eclipse.jgit.lib.ObjectWriter(masterBranch.repository))
    val commitId = createCommit("Test commit", treeId)
    moveMaster(commitId)

    val foundFiles = withDb { db =>
      db.latestSnapshot.all(SimpleMessageFileType).toList
    }
    assertEquals(Nil, foundFiles)
  }

  @Test
  def modifySimpleMessagesWithoutMovingFiles {
    import com.google.gimd.query.Predicate.functionLiteral2Predicate

    val first = SimpleMessage("first", 1)
    val second = SimpleMessage("second", 2)

    val paths = List("sm/first.sm", "sm/second.sm")

    val blobIds = List(writeMessage(SimpleMessageType, first),
                       writeMessage(SimpleMessageType, second))
    commit(paths zip blobIds)

    val foundFiles = withDb { db =>
      db.modify { snapshot =>
        val q = SimpleMessageType.query where { _.name === "second" }
        val sms = snapshot.query(SimpleMessageFileType, q)
        sms.foldLeft(DatabaseModification.empty) {
          case (m, (h, sm)) => m.modify(h, SimpleMessage(sm.name, sm.value+1))
        }
      }
      db.latestSnapshot.all(SimpleMessageFileType).toList
    }

    val expectedMsgs = List(first, SimpleMessage(second.name, second.value+1))
    assertEquals(expectedMsgs, foundFiles.map(_.userObject))
    assertEquals(paths, foundFiles.map(_.path))
  }

  @Test
  def modifySimpleMessagesWithMovingFiles {
    import com.google.gimd.query.Predicate.functionLiteral2Predicate

    val first = SimpleMessage("first", 1)
    val second = SimpleMessage("second", 2)

    val paths = List("sm/first.sm", "sm/second.sm")

    val blobIds = List(writeMessage(SimpleMessageType, first),
                       writeMessage(SimpleMessageType, second))
    commit(paths zip blobIds)

    val db = createDatabase

    val third = SimpleMessage("third", 3)

    val foundFiles = withDb { db =>
      db.modify { snapshot =>
        val q = SimpleMessageType.query where { _.name === "second" }
        val sms = snapshot.query(SimpleMessageFileType, q)
        sms.foldLeft(DatabaseModification.empty) {
          case (m, (h, sm)) => m.modify(h, third)
        }
      }
      db.latestSnapshot.all(SimpleMessageFileType).toList
    }

    val expectedMsgs = List(first, third)
    val expectedPaths = List("sm/first.sm", "sm/third.sm")
    assertEquals(expectedMsgs, foundFiles.map(_.userObject))
    assertEquals(expectedPaths, foundFiles.map(_.path))
  }

  @Test
  def deleteSimpleMessage {
    import com.google.gimd.query.Predicate.functionLiteral2Predicate

    val first = SimpleMessage("first", 1)
    val second = SimpleMessage("second", 2)

    val files = List(
      ("sm/first.sm", writeMessage(SimpleMessageType, first)),
      ("sm/second.sm", writeMessage(SimpleMessageType, second))
    )
    commit(files)

    val db = createDatabase

    val foundFiles = withDb { db =>
      db.modify { snapshot =>
        val q = SimpleMessageType.query where { _.name === "second" }
        val sms = snapshot.query(SimpleMessageFileType, q)
        sms.foldLeft(DatabaseModification.empty) {
          case (m, (h, sm)) => m.remove(h)
        }
      }
      db.latestSnapshot.all(SimpleMessageFileType).toList
    }

    val expected = List(first)
    assertEquals(expected, foundFiles.map(_.userObject))
  }

  @Test
  def emptyModificationAndReturn {
    val expected = 100

    val path = "sm/first.sm"
    val blobId = writeMessage(SimpleMessageType, SimpleMessage("first", 1))

    commit(List(path -> blobId))

    val result = withDb { db =>
      db.modifyAndReturn { _ => (DatabaseModification.empty, expected) }
    }

    assertEquals(expected, result)
  }

  private def commit(files: List[(String, ObjectId)]) {
    val treeId = addFiles(files)
    val commitId = createCommit("Test", treeId)
    moveMaster(commitId)
  }

}
