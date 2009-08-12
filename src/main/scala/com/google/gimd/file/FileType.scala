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

/**
 * FileType carries so meta-information about set of Files that conform to
 * all information carried by FileType.
 */
abstract class FileType[T] {

  /**
   * prefix of File's path or None if there's no common prefix.
   */
  def pathPrefix: Option[String]

  /**
   * suffix of File's path or None if there's no common suffix.
   */
  def pathSuffix: Option[String]

  /**
   * Type of information that File stores.
   */
  def userType: UserType[T]

}
