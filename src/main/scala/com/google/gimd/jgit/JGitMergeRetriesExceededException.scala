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
 * An exception thrown in a case when modifications cannot be applied to JGitDatabase.
 *
 * @see JGitDatabase
 */
class JGitMergeRetriesExceededException(override val branch: JGitBranch, val retries: Int)
        extends JGitDatabaseException(branch, ("Tried to rebase transaction %1s times and " +
                "failed to merge changes successfuly. Aborting.").format(retries))
