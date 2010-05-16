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
import scala.collection.immutable.SortedSet

object Message {
  val empty: Message = new Message(TreeSet.empty(Ordering.ordered[Field]))
  def apply(fields: Iterable[Field]): Message = new Message(TreeSet.empty(Ordering.ordered[Field]) ++ fields)
  def apply(fields: Field*) = new Message(TreeSet(fields: _*)(Ordering.ordered[Field]))
}

//TODO Message should allow to subclass itself only in gimd package but I don't know
//how to do that right now
class Message(private val fields: SortedSet[Field])
        extends Ordered[Message] with SortedSet[Field] {

  override def equals(that: Any) = that match {
    case that: Message => compare(that) == 0
    case _ => false
  }

  val ordering = Ordering.ordered[Field]

  override def compare(that: Message) =
    Ordering.Iterable(ordering).compare(this.fields, that.fields)

  override def rangeImpl(from: Option[Field], until: Option[Field]) = fields.rangeImpl(from, until)
  override def keySet = fields.keySet
  override def lastKey = fields.lastKey
  override def firstKey = fields.firstKey
  override def elements = fields.elements

  def -(elem: Field) = new Message(fields - elem)
  def +(elem: Field) = new Message(fields + elem)
  def contains(elem: Field) = fields contains elem
  def iterator = fields.iterator

  /**
   * Returns all fields having name equal to given.
   *
   * This implementation is efficient because it does take advantage of the fact that Message is
   * Sorted collection so operation is performed in O(log(n)) time.
   */
  def all(name: String): SortedSet[Field] = range(MinimumField(name), MaximumField(name))

  /**
   * Filters fields of returned by method all so the returned list contains fields of only one given
   * type. 
   */
  def allOfVariant[T <: Field](name: String): List[T] = all(name).flatMap {
    x => if (x.isInstanceOf[T]) Some(x.asInstanceOf[T]) else None
  } toList

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

  override def toString = "Message(" + fields + ")"
}
