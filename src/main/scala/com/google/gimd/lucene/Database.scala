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
package com.google.gimd.lucene

import org.apache.lucene.store.Directory
import org.apache.lucene.index.{IndexCommit, IndexReader}
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.lib.{ObjectIdSubclassMap, ObjectId, AnyObjectId}

import com.google.gimd.lucene.Database.{UD_COMMIT, LuceneState}
import com.google.gimd.file.FileType
import com.google.gimd.jgit.JGitBranch
import actors.Actor

/**
 * Database that manages Lucene's Index for a given branch and list of file types.
 *
 * The management is automatic which means it's Database responsibility to keep Index up-to-date
 * and to doing in efficient way.
 */
final class Database(val branch: JGitBranch, val fileTypes: List[FileType[_]]) extends Actor {

  val luceneDirectory: Directory = {
    import java.io.File
    val ldir: File = new File(new File(branch.repository.getDirectory, "lucene"), branch.name)
    org.apache.lucene.store.FSDirectory.open(ldir)
  }

  private var state: ObjectIdSubclassMap[LuceneState] = null

  start()

  def act = {
    state == null
    loop {
      react {
        case targetCommit: AnyObjectId => reply(read(targetCommit))
      }
    }
  }

  /**
   * Obtain an index reader to search through a specific Git commit.
   * <p>
   * This method is <b>not thread safe</b>. Aside from not being thread safe
   * here in this instance, its not thread safe on disk. If two concurrent
   * threads want to read two different unindexed target commits, one will fail
   * to get the lock on the Lucene disk structure.
   *
   * @param targetCommit the commit SHA-1 to search through. If it is not yet
   *        already indexed, it will be indexed on the fly before this method
   *        returns. That could take hours.
   * @return the index reader for the requested commit.
   * @throws IOException
   */
  protected def read(targetCommit: AnyObjectId): IndexReader = {
    if (state == null) {
      state = scan(luceneDirectory)
    }
    var s: LuceneState = state.get(targetCommit)
    if (s == null) {
      s = indexNow(targetCommit)
      state.add(s)
    }
    return IndexReader.open(s.indexCommit, true)
  }

  private[lucene] def newAnalyzer() = new org.apache.lucene.analysis.SimpleAnalyzer

  private def indexNow(targetId: AnyObjectId): LuceneState = {
    val walk: RevWalk = new RevWalk(branch.repository)
    val target: RevCommit = walk.parseCommit(targetId)
    var base: LuceneState = baseState(walk, target)
    if (base != null) {
      return new Indexer(this, fileTypes).incremental(base, target, walk)
    }
    return new Indexer(this, fileTypes).full(target)
  }

  private def baseState(walk: RevWalk, start: RevCommit): LuceneState = {
    walk.markStart(start)
    var c: RevCommit = null
    while ({c = walk.next; c} != null) {
      var s: LuceneState = state.get(c)
      if (s != null) {
        return s
      }
    }
    walk.reset
    import scala.collection.JavaConversions.JIterableWrapper
    for (r <- new JIterableWrapper(walk.getRepository.getAllRefs.values)) {
      if (r.getObjectId != null)
          walk.markStart(walk.parseCommit(r.getObjectId))
    }
    walk.markUninteresting(start)
    var last: LuceneState = null
    while ({c = walk.next; c} != null) {
      var s: LuceneState = state.get(c)
      if (s != null) {
        last = s
      }
    }
    return last
  }

  private def scan(ldb: Directory): ObjectIdSubclassMap[LuceneState] = {
    val r: ObjectIdSubclassMap[LuceneState] = new ObjectIdSubclassMap[LuceneState]
    import org.eclipse.jgit.lib.MutableObjectId
    val id: MutableObjectId = new MutableObjectId
    for (c <- list(ldb)) {
      val userData: java.util.Map[String, String] = c.getUserData
      val name: String = userData.get(UD_COMMIT)
      if (name != null) {
        var add = true
        try {
          id.fromString(name)
        }
        catch {
          case e: IllegalArgumentException => add = false
        }
        if (add)
          r.add(new LuceneState(id, c))
      }
    }
    return r
  }

  private def list(ldb: Directory): List[IndexCommit] = {
    import org.apache.lucene.store.NoSuchDirectoryException
    try {
      import scala.collection.JavaConversions.JIterableWrapper
      return new JIterableWrapper(IndexReader.listCommits(ldb)).toList
    }
    catch {
      case notFound: NoSuchDirectoryException => {
        return Nil
      }
    }
  }

}

object Database {

  private[lucene] val UD_COMMIT = "commit"

  /**
   * State of a single version of the Lucene index on disk. The SHA-1 of
   * this object is the git commit SHA-1.
   */
  private[lucene] final class LuceneState(val commitId: AnyObjectId, val indexCommit: IndexCommit)
          extends ObjectId(commitId)

}
