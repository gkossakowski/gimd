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

import org.apache.lucene.index._
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.revwalk.{RevWalk, RevCommit}
import java.util.HashMap
import com.google.gimd.file.{File, FileType}
import com.google.gimd._
import com.google.gimd.jgit.{JGitFile, FileTypeTreeFilter}
import Database.LuceneState
import Indexer._

/**
 * Indexer class that implements updates to the index maintained by a given Database.
 *
 * The Indexer class implements two strategies for updating the index:
 *   a) full
 *   b) incremental
 *
 * Both of them take target parameter which is Git's commit. They will scan for all files in a tree
 * pointed by given commit and will index the contents of these files.
 */
final class Indexer(db: Database, fileTypes: List[FileType[_]]) {

  private final val deletePolicy = new NeverDeletePolicy
  private final val fieldLen = IndexWriter.MaxFieldLength.LIMITED

  private final val lucDb: org.apache.lucene.store.Directory = db.luceneDirectory

  private final val analyzer: org.apache.lucene.analysis.Analyzer = db.newAnalyzer

  private final val tree = new TreeWalk(db.branch.repository)

  private var writer: IndexWriter = null

  private[lucene] def full(target: RevCommit): LuceneState = {
    import java.io.IOException
    if (IndexReader.indexExists(lucDb))
      throw new IOException("index already exists, cannot do full")
    writer = new IndexWriter(lucDb, analyzer, true, deletePolicy, fieldLen)
    //this is dummy command that enforces commit when there are no other changes to commit
    //i.e. in case of indexing an empty repository
    writer.deleteAll();
    for (ft <- fileTypes) {
      tree.reset(target.getTree)
      tree.setFilter(FileTypeTreeFilter(ft))
      tree.setRecursive(true)
      while (tree.next) {
        if ((tree.getRawMode(0) & 0170000) == 0100000) {
          val file = new JGitFile(tree.getPathString, tree.getObjectId(0), ft, db.branch)
          scanFile(file)
        }
      }
    }
    return commit(target)
  }

  private[lucene] def incremental(state: LuceneState, target: RevCommit, walk: RevWalk):
    LuceneState = {
    import org.eclipse.jgit.treewalk.filter.{AndTreeFilter, TreeFilter}
    import org.eclipse.jgit.lib.AnyObjectId
    val ic: IndexCommit = state.indexCommit
    writer = new IndexWriter(lucDb, analyzer, deletePolicy, fieldLen, ic)
    val base: RevCommit = walk.parseCommit(state)
    for (ft <- fileTypes) {
      tree.reset(Array[AnyObjectId](target.getTree, base.getTree))
      tree.setFilter(AndTreeFilter.create(TreeFilter.ANY_DIFF, FileTypeTreeFilter(ft)))
      tree.setRecursive(true)
      while (tree.next) {
        val targetMode: Int = tree.getRawMode(0)
        val baseMode: Int = tree.getRawMode(1)
        if (baseMode != 0 && targetMode == 0) {
          writer.deleteDocuments(new Term(PATH_NAME, tree.getPathString))
        }
        else if ((targetMode & 0170000) == 0100000) {
          writer.deleteDocuments(new Term(PATH_NAME, tree.getPathString))
          val file = new JGitFile(tree.getPathString, tree.getObjectId(0), ft, db.branch)
          scanFile(file)
        }
      }
    }
    return commit(target)
  }

  private def commit(target: RevCommit): LuceneState = {
    val lcommit = new HashMap[String, String]
    lcommit.put(Database.UD_COMMIT, target.name)
    writer.commit(lcommit)
    val state = writer.getReader.getIndexCommit
    writer.close
    new LuceneState(target, state)
  }

  private def scanFile(file: File[_]) {
    writer.deleteDocuments(new Term(PATH_NAME, file.path))

    scanMessage(file.path, file.message, file.fileType.userType)
  }

  private def scanMessage(filePath: String, msg: Message, ut: UserType[_]) {
    import org.apache.lucene.document.{Document, Field => LField}
    import org.apache.lucene.document.Field.{Store, Index, TermVector}

    def stringValues(f: FieldSpec[_,_]) = msg.all(f.name) map (_.stringField.value)
    def field(name: String, value: String) =
      new LField(name, value, Store.YES, Index.NOT_ANALYZED, TermVector.NO)

    val valueFields = for {
      fieldSpec <- ut.fields
      sv <- stringValues(fieldSpec)
    } yield field(fieldSpec.name, sv)

    val pathField = field(PATH_NAME, filePath)
    val utNameField = field(UT_NAME, ut.getClass.getName)
    val allFields = pathField :: utNameField :: valueFields

    val doc = new Document
    for (f <- allFields)
      doc.add(f)
    writer.addDocument(doc)

    for {
      child <- ut.children
      childMsgField <- msg.allOfVariant[MessageField](child.name)
    } scanMessage(filePath, childMsgField.value, child.userType)
  }


  final class NeverDeletePolicy extends IndexDeletionPolicy {
    def onInit(commits: java.util.List[_ <: IndexCommit]): Unit = ()

    def onCommit(arg0: java.util.List[_ <: IndexCommit]): Unit = ()
  }

}

object Indexer {

  val PATH_NAME = "$path"
  val UT_NAME = "$userType"

}
