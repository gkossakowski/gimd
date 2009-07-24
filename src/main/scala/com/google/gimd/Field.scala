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

object Field {
  def apply(name: String, value: Int)        = IntField(name, value)
  def apply(name: String, value: Long)       = LongField(name, value)
  def apply(name: String, value: BigInt)     = BigIntField(name, value)
  def apply(name: String, value: BigDecimal) = BigDecimalField(name, value)
  def apply(name: String, value: Timestamp)  = TimestampField(name, value)
  def apply(name: String, value: String)     = StringField(name, value)
  def apply(name: String, value: Message)    = MessageField(name, value)
}

abstract sealed case class Field() extends Ordered[Field] with Comparable[Field] {
  def name: String

  def compare(that: Field) = {
    val c = this.name.compare(that.name)
    if (c != 0)
      c
    else
      compareValue(this, that)
  }

  private def compareValue(a: Field, b: Field) = (a, b) match {
    case (a: IntField,       b: IntField)        => a.value compare b.value
    case (a: LongField,      b: LongField)       => a.value compare b.value
    case (a: BigIntField,    b: BigIntField)     => a.value compare b.value
    case (a: NumberField,    b: NumberField)     => cmp(a.toBigDecimal, b.toBigDecimal)

    case (a: TimestampField, b: TimestampField)  => a.value compareTo b.value
    case (a: StringField,    b: StringField)     => a.value compare b.value
    case (a: MessageField,   b: MessageField)    => a.value compare b.value

    case (a, b) => a.domainOrder compare b.domainOrder
  }

  private def domainOrder = this match {
    case a: NumberField     => 0
    case a: TimestampField  => 1
    case a: StringField     => 2
    case a: MessageField    => 3
  }

  private def cmp(a: BigDecimal, b: BigDecimal) = {
    val scale = a.scale max b.scale
    a.setScale(scale) compare b.setScale(scale)
  }
}

sealed case class StringField(name: String, value: String) extends Field
sealed case class TimestampField(name: String, value: Timestamp) extends Field
sealed case class MessageField(name: String, value: Message) extends Field

abstract sealed case class NumberField() extends Field {
  def toNumber: Number
  def toBigDecimal: BigDecimal
}
sealed case class IntField(name: String, value: Int) extends NumberField {
  def toNumber = value
  def toBigDecimal = BigDecimal(value)
}
sealed case class LongField(name: String, value: Long) extends NumberField {
  def toNumber = value
  def toBigDecimal = BigDecimal(value)
}
sealed case class BigIntField(name: String, value: BigInt) extends NumberField {
  def toNumber = value
  def toBigDecimal = BigDecimal(value)
}
sealed case class BigDecimalField(name: String, value: BigDecimal) extends NumberField {
  def toNumber = value
  def toBigDecimal = value
}
