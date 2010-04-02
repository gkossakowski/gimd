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

import com.google.gimd.{Message, UserType}

/**
 * FileType carries so meta-information about set of Files that conform to
 * all information carried by FileType.
 */
abstract class FileType[T] {

  //TODO: This assertion should be checked but for now it has to be commented out as there is some
  //TODO: problem with initialization order. NPEs is being thrown because assertion is being checked
  //TODO: before subclass initializes pathPrefix field. Need to figure out why this happens.
  //assert(pathPrefix.map(_ endsWith "/").getOrElse(true))

  /**
   * prefix of File's path or None if there's no common prefix.
   * For Some(x) condition x.last == "/" must be satisfied.
   */
  val pathPrefix: Option[String]

  /**
   * suffix of File's path or None if there's no common suffix.
   */
  val pathSuffix: Option[String]

  /**
   * Function that maps given Message to a name of a file where Message can be stored.
   */
  protected def name(m: Message): String

  /**
   * Returns full path of a file where given Message can be stored.
   *
   * @see #name(m: Message)
   */
  final def path(m: Message) = pathPrefix.getOrElse("") + name(m) + pathSuffix.getOrElse("")

  /**
   * Type of information that File stores.
   */
  val userType: UserType[T]

}
