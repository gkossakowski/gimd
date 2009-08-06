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
import scala.collection.immutable.TreeSet
import scala.collection.Sorted

object Message {
  val empty: Message = new Message(TreeSet.empty)
  def apply(fields: Iterable[Field]) = empty ++ fields
  def apply(fields: Field*) = new Message(TreeSet(fields: _*))

  def filterMessageFields(xs: Iterable[Field]): List[Message] =
    xs.flatMap(_ match {
      case x: MessageField => List(x.value)
      case _ => Nil
    }).toList
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

  /**
   * Returns all fields having name equal to given.
   *
   * This implementation is efficient because it does take advantage of the fact that Message is
   * Sorted collection so operation is performed in O(log(n)) time.
   */
  def all(name: String): Sorted[Field,Field] = range(MinimumField(name), MaximumField(name))

  /**
   * @throws Predef.NoSuchElementException if there is more than one field with given name
   */
  def oneOption(name: String): Option[Field] = all(name).take(2).toList match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => throw new NoSuchElementException("There are more than one " +
                                                     "fields named " + name)
  }

  /**
   * Returns field with passed name.
   *
   * @throws Predef.NoSuchElementException if either there's no element with given name or
   * there are more than one
   */
  def one(name: String): Field = oneOption(name).get

  override def toString = Formatter.format(this)
}
