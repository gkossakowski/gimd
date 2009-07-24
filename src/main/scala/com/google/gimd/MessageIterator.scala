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

import com.google.gimd.text.Formatter

class MessageIterator(msg: Message) {
  private val i = msg.elements
  private var cur: Option[Field] = None
  private var retry = false
  next()

  def is(name: String): Boolean = cur match {
    case Some(f) => name.equals(f.name)
    case _ => false
  }

  def hasMore = {
    if (!retry)
      next()
    retry = false
    cur match {
      case Some(_) => true
      case _ => i.hasNext
    }
  }

  private def next() = cur = if (i.hasNext) Some(i.next()) else None
  private def use = cur match {
    case Some(f) => { 
      retry = true
      next()
      f
    }
    case _ =>
      throw new IllegalStateException("No current field")
  }

  def getInt: Int = use match {
    case f: IntField => f.value
    case f => throw new TypeMismatchError(classOf[IntField], f)
  }
  def getLong: Long = use match {
    case f: LongField => f.value
    case f: IntField  => f.value.longValue
    case f => throw new TypeMismatchError(classOf[LongField], f)
  }
  def getBigInt: BigInt = use match {
    case f: BigIntField => f.value
    case f: LongField   => BigInt(f.value)
    case f: IntField    => BigInt(f.value)
    case f => throw new TypeMismatchError(classOf[BigIntField], f)
  }
  def getBigDecimal: BigDecimal = use match {
    case f: NumberField => f.toBigDecimal
    case f => throw new TypeMismatchError(classOf[BigDecimalField], f)
  }
  def getString: String = use match {
    case f: StringField => f.value
    case f: NumberField => Formatter.valueString(f)
    case f => throw new TypeMismatchError(classOf[StringField], f)
  }
  def getMessage: Message = use match {
    case f: MessageField => f.value
    case f => throw new TypeMismatchError(classOf[MessageField], f)
  }
}
