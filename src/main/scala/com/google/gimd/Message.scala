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

import collection.immutable.TreeSet
import collection.Sorted
import com.google.gimd.text.Formatter

object Message {
  val empty: Message = new Message(TreeSet.empty)
  def apply(fields: Iterable[Field]) = empty ++ fields
  def apply(fields: Field*) = new Message(TreeSet(fields: _*))
}

final class Message(private val fields: Sorted[Field, Field])
        extends Ordered[Message] with Sorted[Field, Field] {

  override def equals(that: Any) = that match {
    case that: Message => compare(that) == 0
    case _ => false
  }

  override def compare(that: Message) = iterable2ordered(this.fields).compare(that.fields)
  override def compare(k0: Field, k1: Field) = k0.compare(k1)  

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

  override def rangeImpl(from: Option[Field], until: Option[Field]) = fields.rangeImpl(from, until)
  override def keySet = fields.keySet
  override def lastKey = fields.lastKey
  override def firstKey = fields.firstKey
  override def elements = fields.elements

  def iterator = new MessageIterator(this)

  override def toString = Formatter.format(this)
}
