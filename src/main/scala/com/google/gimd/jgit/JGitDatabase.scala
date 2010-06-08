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

import com.google.gimd.{Database, Message, Snapshot}
import com.google.gimd.file.FileType
import com.google.gimd.modification.DatabaseModification
import com.google.gimd.text.Formatter
import org.eclipse.jgit.lib._
import org.eclipse.jgit.merge.MergeStrategy
import org.eclipse.jgit.lib.RefUpdate.Result
import org.eclipse.jgit.dircache.{DirCache, DirCacheEditor, DirCacheEntry}
import org.eclipse.jgit.revwalk.{RevCommit, RevTree, RevWalk}
import java.io.IOException

class JGitDatabase(val fileTypes: List[FileType[_]], val branch: JGitBranch) extends Database {

  /**
   * The maximal number of merge/transaction rebase retries.
   *
   * If this number of retries is reached attempt to apply modifications will be aborted by throwing
   * an exception.
   *
   * This boundary is needed in order to avoid resource starvation, specifically - livelock. It may
   * happen when there is another process constantly changing latest Snapshot so modification has to
   * be constantly remerged or even rebased. Proper solution to this problem should be fixing
   * scheduling algorithm that would assign priority to each modification task which is proportional
   * to number of retries given task already performed.
   *
   * Unfortunately we don't have control over scheduling threads here.
   */
  private val MERGE_RETRIES = 10

  protected val repository = branch.repository

  def latestSnapshot: JGitSnapshot = {
    try {
      new JGitSnapshot(branch, latestCommitId)
    } catch {
      case e: IOException => throw new JGitDatabaseException(branch, e)
    }
  }

  def modifyAndReturn[T](modification: Snapshot => (DatabaseModification, T)): T =
    modifyAndReturnWithCommit(modification)._1

  protected def modifyAndReturnWithCommit[T](modification: Snapshot => (DatabaseModification, T)):
  (T, ObjectId) = {
    val result = try {
      retry(MERGE_RETRIES) {
        val snapshot = latestSnapshot
        val (dbModification, result) = modification(snapshot)
        applyModification(dbModification, snapshot.commit).map((result, _))
      }
    } catch {
      case e: IOException => throw new JGitDatabaseException(branch, e)
    }

    result getOrElse { throw new JGitMergeRetriesExceededException(branch, MERGE_RETRIES) }
  }

  def applyModification(modification: DatabaseModification, onto: RevCommit): Option[ObjectId] = {
    val treeId = writeMessages(modification, onto.getTree)
    val commitId = createCommit(treeId, onto)
    val result = updateRef(onto, commitId)
    result match {
      //there was no change to database since onto commit so changes were applied cleanly
      case Result.FAST_FORWARD => Some(commitId)
      //if there was a change since onto commit update gets rejected. Still it might be that change
      //did not affected files we are trying to modify. Thus we are trying merging both changes.
      case Result.REJECTED => tryMerging(commitId)
      //NO_CHANGE is returned in case that both onto and commitId are the same. From Gimd's point of
      //view it means that some concurrent change has been applied that brought Gimd's db to the
      //exactly the same state we are trying to commit here. The only way to handle this case is to
      //reset the transaction. We do that by returning None which means that current modification
      //cannot be applied and the transaction has to be restarted
      case Result.NO_CHANGE => None
      //TODO: There should be a special treatment of LOCK_FAILURE case but it's low-priority task
      //the idea is to not try merging if just JGit fails to obtain lock for given reference
      case _ =>
        throw new JGitDatabaseException(branch,
                                        "RefUpdate returned unexpected result: %1s.".format(result))
    }
  }

  /**
   * Returns latest commit id that our branch is pointing at.
   */
  protected def latestCommitId: RevCommit = {
    val id = repository.resolve(branch.name)
    new RevWalk(repository).parseCommit(id)
  }

  val updateRefActor = new UpdateRefActor(branch)

  private def updateRef(oldCommit: ObjectId, newCommit: ObjectId): Result = {
    val TIMEOUT = 10000
    val msg = UpdateRefActor.UpdateRef(newCommit)
    val awaitAnswer = updateRefActor.awaitAnswer(TIMEOUT)
    awaitAnswer.!?(TIMEOUT, msg).asInstanceOf[Option[Result]] getOrElse Result.LOCK_FAILURE
  }

  private def createCommit(treeId: ObjectId, parents: ObjectId*): ObjectId = {
    //TODO: This identity should be replaced with something more meaningful
    val ident = new PersonIdent("A U Thor", "author@example.com")
    val commit = new Commit(repository, Array(parents: _*))
    commit.setTreeId(treeId)
    commit.setAuthor(ident)
    commit.setCommitter(ident)
    commit.setMessage("Comitted by Gimd.\n")
    commit.commit()
    commit.getCommitId
  }

  private def writeMessages(modification: DatabaseModification, oldTree: RevTree): ObjectId = {
    val dirCache = DirCache.newInCore
    val builder = dirCache.builder
    builder.addTree(new Array[Byte](0), 0, repository, oldTree)
    builder.finish

    val objectWriter = new ObjectWriter(repository)
    class WriteMessage(path: String, msg: Message) extends DirCacheEditor.PathEdit(path) {
      def apply(entry: DirCacheEntry) {
        val text = Formatter.format(msg)
        val blobId = objectWriter.writeBlob(text.getBytes(Constants.CHARACTER_ENCODING))
        entry.setFileMode(FileMode.REGULAR_FILE)
        entry.setObjectId(blobId)
      }
    }
    val editor = dirCache.editor
    val (modified, newFiles) = modification.reduce
    for ((file, message) <- modified) {
      message match {
        case None => editor.add(new DirCacheEditor.DeletePath(file.path))
        case Some(x) => {
          val newPath = file.fileType.path(x)
          if (newPath != file.path)
            //TODO: It's undefined what will happen if one file will get renamed and another file
            //TODO: is added in place of old file.
            editor.add(new DirCacheEditor.DeletePath(file.path))
          editor.add(new WriteMessage(newPath, x))
        }
      }
    }
    for ((fileType, message) <- newFiles) {
      editor.add(new WriteMessage(fileType.path(message), message))
    }
    editor.finish
    dirCache.writeTree(objectWriter)
  }

  private def merge(commitToBeMerged: ObjectId): Option[ObjectId] = {
    val baseCommit = repository.resolve(branch.name)
    val merger = MergeStrategy.SIMPLE_TWO_WAY_IN_CORE.newMerger(repository)
    try {
      if (merger.merge(baseCommit, commitToBeMerged)) {
        val treeId = merger.getResultTreeId
        val mergeCommit = createCommit(treeId, baseCommit, commitToBeMerged)
        val result = updateRef(baseCommit, mergeCommit)
        if (Result.FAST_FORWARD == result) Some(mergeCommit) else None
      } else
        None
    } catch {
      case e: IOException if e.getMessage.contains("Multiple merge bases") => None
    }
  }

  private def tryMerging(commitToBeMerged: ObjectId): Option[ObjectId] =
    retry(MERGE_RETRIES){ merge(commitToBeMerged) }

  private def retry[T](howManyTimes: Int)(what: => Option[T]): Option[T] =
    if (howManyTimes > 0)
      what match {
        case Some(x) => Some(x)
        case None => retry(howManyTimes-1)(what)
      }
    else
      None

}
