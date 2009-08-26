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

import file.{File, FileType}
import query.{Predicate, Handle}

/**
 * Trait that provides all functionality for Gimd to ask specific storage implementation
 * for needed data in an efficient way.
 */
trait DatabaseSpi {

  /**
   * @return iterator over collection of all Files that conform to passed FileType[T].
   */
  def all[T](fileType: FileType[T]): Iterator[File[T]]

  /**
   * Query database for all user objects of type U stored in files of
   * type FileType[W] satisfying predicate p.
   */
  def query[U,W](ft: FileType[W], p: Predicate[U]): Iterator[(Handle,U)] = {
    for {
      f <- all(ft)
      r <- f.query(p)
    } yield r
  }

}