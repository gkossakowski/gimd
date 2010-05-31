// Copyright (C) 2010 The Android Open Source Project
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

import file.FileType
import jgit.{JGitDatabase, JGitBranch}
import lucene.LuceneOptimizedDatabase

/**
 * Simple helper object containing some methods useful when using Gimd in console mode.
 */
object GimdConsole {

  import query.Query._

  def openDb(path: String, fileTypes: List[FileType[_]]): Database =
    new JGitDatabase(fileTypes, JGitUtils.createRepository(path)) with LuceneOptimizedDatabase

  def measureTime(n: Int, action: () => Unit): (Double, Double) = {
    import java.util.Date
    val times = (1 to n) map { _ =>
      val time1 = (new Date).getTime
      action()
      val time2 = (new Date).getTime
      time2-time1
    }
    val avg = times.sum / n
    val variance = times.map(x => (avg - x)*(avg - x)).sum / n
    (avg, variance)
  }


  protected object JGitUtils {
    import org.spearce.jgit.lib.{PersonIdent, ObjectId, Commit, Repository, ObjectWriter, Constants}

    protected val masterRef = Constants.R_HEADS + Constants.MASTER

    def createRepository(path: String): JGitBranch = {
      val file = new java.io.File(path + "/.git")
      val repository = new Repository(file)
      val masterBranch = JGitBranch(repository, masterRef)
      if (!file.exists) {
        file.mkdirs()
        repository.create()
        val treeId = addFiles(repository, Nil)
        val commitId = createCommit(repository, "Initial commit", treeId)
        val refUpdate = masterBranch.repository.updateRef(masterRef)
        refUpdate.setNewObjectId(commitId)
        refUpdate.forceUpdate()
      }
      masterBranch
    }

    private def createCommit(repository: Repository, message: String, treeId: ObjectId):
    ObjectId = {
      val commit = new Commit(repository, Array())
      val person = new PersonIdent("A U Thor", "author@example.com")
      commit.setAuthor(person)
      commit.setCommitter(person)
      commit.setMessage(message)
      commit.setTreeId(treeId)
      commit.commit()
      commit.getCommitId
    }

    protected def addFiles(repository: Repository, files: List[(String, ObjectId)]): ObjectId = {
      val dc = org.spearce.jgit.dircache.DirCache.newInCore
      val builder = dc.builder
      for ((path, blobId) <- files) {
        val entry = new org.spearce.jgit.dircache.DirCacheEntry(path)
        entry.setFileMode(org.spearce.jgit.lib.FileMode.REGULAR_FILE)
        entry.setObjectId(blobId)
        builder.add(entry)
      }
      builder.finish()
      dc.writeTree(new ObjectWriter(repository))
    }

  }

}
