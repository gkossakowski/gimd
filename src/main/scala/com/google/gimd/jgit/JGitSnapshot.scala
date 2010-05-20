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

import com.google.gimd.Snapshot
import com.google.gimd.file.{File, FileType}
import org.spearce.jgit.revwalk.RevCommit
import org.spearce.jgit.treewalk.TreeWalk

class JGitSnapshot(val branch: JGitBranch, val commit: RevCommit) extends Snapshot {

  def all[T](fileType: FileType[T]): Iterator[File[T]] = {
    val treeWalk = new TreeWalk(branch.repository)
    treeWalk.reset(commit.getTree)
    treeWalk.setRecursive(true)
    treeWalk.setFilter(FileTypeTreeFilter(fileType))

    new TreeWalkIterator(fileType, treeWalk)
  }

  protected final class TreeWalkIterator[T](val fileType: FileType[T], val treeWalk: TreeWalk)
          extends Iterator[File[T]] {
    private var doesHasNext = treeWalk.next
    def hasNext = doesHasNext
    def next = {
      if (!hasNext)
        throw new NoSuchElementException
      val result =
        new JGitFile(treeWalk.getPathString, treeWalk.getObjectId(0), fileType, branch)
      doesHasNext = treeWalk.next
      result
    }
  }

}
