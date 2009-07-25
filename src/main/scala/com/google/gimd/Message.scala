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

package com.google.gimd;

import com.google.gimd.text.Formatter

object Message {
  val empty: Message = new ImmutableMessage(RandomAccessSeq.empty)
  def apply(fields: Seq[Field]) = empty ++ fields
  def apply(fields: Field*) = new ImmutableMessage(fields.toArray)
}

abstract class Message() extends Ordered[Message] with RandomAccessSeq[Field] {
  protected def sortedFields: RandomAccessSeq[Field]

  override def equals(that: Any) = that match {
    case that: Message => compare(that) == 0
    case _ => false
  }

  def length = sortedFields.length
  def apply(index: Int) = sortedFields(index)
  def compare(that: Message) = this.sortedFields.compare(that.sortedFields)

  def get(index: Int): Field = this(index)

  def +(field: Field): Message = {
    val buffer = new MessageBuffer
    buffer ++= this
    buffer += field
    buffer.readOnly
  }

  def ++(that: Iterable[Field]): Message = {
    val buffer = new MessageBuffer
    buffer ++= this
    buffer ++= that
    buffer.readOnly
  }

  def iterator = new MessageIterator(this)
  def readOnly: ImmutableMessage
  override def toString = Formatter.format(this)
}

final class ImmutableMessage(val myFields: RandomAccessSeq[Field]) extends Message {
  protected def sortedFields = myFields
  def readOnly = this
}
