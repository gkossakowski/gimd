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

import org.spearce.jgit.lib.Repository

/**
 * <p>Class that stores information about branch which consists of name of branch and repository
 * where that branch is stored.</p>
 *
 * @throws InvalidJGitBranchNameException if supplied <code>name</code> does not pass
 *         <code>Repository.isValidRefName</code> test.
 */
@throws(classOf[InvalidJGitBranchNameException])
final case class JGitBranch(repository: Repository, name: String) {
   if (!Repository.isValidRefName(name))
     throw new InvalidJGitBranchNameException(repository, name)
}
