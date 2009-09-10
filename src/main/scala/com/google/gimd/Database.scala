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

package com.google.gimd

import file.FileType
import modification.DatabaseModification
import query.Predicate

trait Database {

  /**
   * Query database for all user objects of type U stored in files of
   * type FileType[W] satisfying predicate p using latest Snapshot of Database.
   *
   * @throws GimdException
   */
  @throws(classOf[GimdException])
  def query[U,W](ft: FileType[W], p: Predicate[U]): Iterator[U] =
    //this method forgets about handles as this method is not run in context of any Snapshot so they
    //become invalid immediately
    latestSnapshot.query(ft, p).map(_._2)

  /**
   * <p>Method that allows modification of latest Snapshot of Database.</p>
   *
   * <p>For single DatabaseModification derived from Snapshot READ COMMITTED level of isolation is
   * guaranteed. <strong>The level of isolation might be changed in a future but only to level which
   * provides better isolation.</strong></p>
   *
   * @param modification A function <code>Snapshot => DatabaseModification</code> that should
   *                     follow referential transparency rule as it can be called an arbitrary
   *                     number of times. Result of this function is an object that specifies what
   *                     kind of modifications should be performed on top of passed Snapshot.
   *
   * @throws GimdException
   */
  @throws(classOf[GimdException])
  def modify(modification: Snapshot => DatabaseModification)

  /**
   * Factory method returning latest Snapshot of Database.
   */
  protected def latestSnapshot: Snapshot

}
