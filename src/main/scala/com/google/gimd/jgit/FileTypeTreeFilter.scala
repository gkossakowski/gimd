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
package com.google.gimd.jgit

import org.eclipse.jgit.lib.FileMode
import com.google.gimd.file.FileType
import org.eclipse.jgit.treewalk.filter.{AndTreeFilter, PathFilter, PathSuffixFilter, TreeFilter}
import org.eclipse.jgit.treewalk.TreeWalk


object FileTypeTreeFilter {

  def apply[T](fileType: FileType[T]): TreeFilter =
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
