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

import com.google.gimd.{Database, UserType}
import com.google.gimd.file.FileType
import com.google.gimd.text.Formatter
import org.eclipse.jgit.lib._
import org.junit.{After, Before}
import java.io.{ByteArrayInputStream, IOException, File}

abstract class AbstractJGitTestCase {

  private val repositoryPath = "test-repository"

  protected val masterRef = Constants.R_HEADS + Constants.MASTER

  protected var masterBranch: JGitBranch = null

  /**
   * Option determining weather repository created by this test-case will be deleted at the end
   * of test-case execution.
   */
  protected val deleteRepository = true

  protected val fileTypes: List[FileType[_]]

  protected def createDb = new JGitDatabase(fileTypes, masterBranch)

  protected def withDb[T](f: JGitDatabase => T): T = {
    val db = createDb
    try {
      f(db)
    } finally {
      db.close()
    }
  }

  @Before protected def createRepository {
    val file = new File(repositoryPath + "/.git")
    val repository = new Repository(file)
    masterBranch = JGitBranch(repository, masterRef)
    if (!file.exists) {
      file.mkdirs()
      repository.create()
      val treeId = addFiles(Nil)
      val commitId = createCommit("Initial commit", treeId)
      moveMaster(commitId)
    }
  }

  @After protected def removeRepository {
    if (deleteRepository)
      org.apache.commons.io.FileUtils.deleteDirectory(new File(repositoryPath))
  }

  protected def writeTextContent(text: String): ObjectId = {
    val ow = new ObjectWriter(masterBranch.repository)
    ow.writeBlob(text.getBytes("UTF-8"))
  }

  protected def writeMessage[U](userType: UserType[U], userObject: U): ObjectId =
    writeTextContent(Formatter.format(userType.toMessageBuffer(userObject).readOnly))

  protected def addFiles(files: List[(String, ObjectId)]): ObjectId = {
    val dc = org.eclipse.jgit.dircache.DirCache.newInCore
    val builder = dc.builder
    for ((path, blobId) <- files) {
      val entry = new org.eclipse.jgit.dircache.DirCacheEntry(path)
      entry.setFileMode(org.eclipse.jgit.lib.FileMode.REGULAR_FILE)
      entry.setObjectId(blobId)
      builder.add(entry)
    }
    builder.finish()
    dc.writeTree(new ObjectWriter(masterBranch.repository))
  }

  protected def createCommit(message: String, treeId: ObjectId): ObjectId = {
    val commit = new Commit(masterBranch.repository, Array())
    val person = new PersonIdent("A U Thor", "author@example.com")
    commit.setAuthor(person)
    commit.setCommitter(person)
    commit.setMessage(message)
    commit.setTreeId(treeId)
    commit.commit()
    commit.getCommitId
  }

  protected def moveMaster(commitId: ObjectId) {
    val refUpdate = masterBranch.repository.updateRef(masterRef)
    refUpdate.setNewObjectId(commitId)
    refUpdate.forceUpdate()
  }

}
