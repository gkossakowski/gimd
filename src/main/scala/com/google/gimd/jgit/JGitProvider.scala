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

import file.{FileType, File}
import org.spearce.jgit.lib._
import org.spearce.jgit.revwalk.{RevCommit, RevWalk}
import org.spearce.jgit.treewalk.filter.{AndTreeFilter, PathFilter, PathSuffixFilter}
import org.spearce.jgit.treewalk.filter.TreeFilter
import org.spearce.jgit.treewalk.TreeWalk

final class JGitProvider(val jgitRepository: Repository) extends DatabaseSpi {

  def all[T](fileType: FileType[T]): Iterator[File[T]] = {
    val id = jgitRepository.resolve(Constants.HEAD)
    val rw = new RevWalk(jgitRepository)
    all(fileType, rw.parseCommit(id))
  }

  private def all[T](fileType: FileType[T], commit: RevCommit): Iterator[File[T]] = {
    val treeWalk = new TreeWalk(jgitRepository)
    treeWalk.reset(commit.getTree)
    treeWalk.setRecursive(true)
    treeWalk.setFilter(treeFilter(fileType))

    val fileIterator = new Iterator[File[T]] {
      private var doesHasNext = treeWalk.next
      def hasNext = doesHasNext
      def next = {
        if (!hasNext)
          throw new NoSuchElementException
        val result =
          new JGitFile(treeWalk.getPathString, treeWalk.getObjectId(0), fileType, jgitRepository)
        doesHasNext = treeWalk.next
        result
      }
    }
    fileIterator
  }

  private def treeFilter[T](fileType: FileType[T]): TreeFilter =
    AndTreeFilter.create(
      Array(
        RegularFileFilter,
        fileType.pathPrefix.map(PathFilter.create(_)).getOrElse(TreeFilter.ALL),
        fileType.pathSuffix.map(PathSuffixFilter.create(_)).getOrElse(TreeFilter.ALL)
      )
    )

  private object RegularFileFilter extends TreeFilter {
    def shouldBeRecursive = false
    def include(treeWalk: TreeWalk) =
      treeWalk.isSubtree || FileMode.REGULAR_FILE.equals(treeWalk.getFileMode(0))
    override def clone = this
  }

}
