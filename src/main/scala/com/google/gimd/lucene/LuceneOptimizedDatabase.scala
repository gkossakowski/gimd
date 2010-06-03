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

import java.io.IOException
import com.google.gimd.jgit.{JGitDatabaseException, JGitSnapshot, JGitDatabase}
import com.google.gimd.Snapshot
import com.google.gimd.modification.DatabaseModification

trait LuceneOptimizedDatabase extends JGitDatabase {

  protected val luceneDb = new Database(branch, fileTypes)

  override def latestSnapshot: JGitSnapshot = {
    try {
      new JGitSnapshot(branch, latestCommitId) with LuceneOptimizedSnapshot {
        val luceneDb = LuceneOptimizedDatabase.this.luceneDb
      }
    } catch {
      case e: IOException => throw new JGitDatabaseException(branch, e)
    }
  }

  override def modifyAndReturn[T](modification: Snapshot => (DatabaseModification, T)): T = {
    val (result, commitId) = super.modifyAndReturnWithCommit(modification)
    (luceneDb ! commitId)
    result
  }

}
