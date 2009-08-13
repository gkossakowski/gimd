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
import java.io.{InputStreamReader, ByteArrayInputStream}
import org.spearce.jgit.lib.{ObjectId, Repository}
import text.Parser

final class JGitFile[T](val path: String, val blobId: ObjectId, val fileType: FileType[T],
                        val jgitRepository: Repository) extends File[T] {

  lazy val message = Parser.parse(new InputStreamReader(blobInputStream, "UTF-8"))
  lazy val userObject = fileType.userType.toUserObject(message)

  private lazy val blobInputStream = {
    val objectLoader = jgitRepository.openBlob(blobId)
    if (objectLoader == null)
      throw new JGitDatabaseException("Blob '" + blobId.name + "' does not exist.")

    new ByteArrayInputStream(objectLoader.getCachedBytes)
  }

}
