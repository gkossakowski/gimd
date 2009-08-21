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

package com.google.gimd.query

import com.google.gimd.file.File

/** Any handle to a stored message. */
abstract sealed class Handle

/** Handle to a message contained within something else. */
abstract case class InnerHandle() extends Handle

/** Handle to a top-level message, that is not nested. */
abstract case class RootHandle() extends Handle

/** Handle to a message by itself. */
final case class MessageHandle(
  message: Message
) extends InnerHandle

/** Handle to a message stored within a field. */
final case class FieldHandle(
  // TODO We may be able to do a more efficient binding to the field
  // in the parent by taking advantage of position of field in the
  // parent, rather than using the object.
  //
  field: MessageField,
  child: InnerHandle
) extends InnerHandle

/** Handle to a top level message. */
final case class FileHandle[T](
  file: File[T],
  child: InnerHandle
) extends RootHandle
