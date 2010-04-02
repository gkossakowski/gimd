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

package com.google.gimd.file

import com.google.gimd.Message
import com.google.gimd.query._

/**
 * File is one of the fundamental concepts in Gimd. Basically File is unit of
 * storage with path associated with it.
 *
 * Every file has exactly one FileType[T] associated and conforms to meta-data
 * carried by FileType[T].
 *
 * As consequence of association to FileType[T] single File[T] stores single message
 * which is representation of type T.
 */
trait File[T] {
  val path: String
  val fileType: FileType[T]
  val message: Message
  val userObject: T

  def query[U](p: Predicate[U]): Iterator[(CompleteHandle[U], U)] =
    for {
      (pathHandle, obj) <- MessageQuery.simpleQuery(fileType.userType, message, p)
    } yield (CompleteHandle(this, pathHandle), obj)
}
