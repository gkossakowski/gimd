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

import scala.util.Sorting
import scala.collection.mutable.ArrayBuffer

class MessageBuffer {
  private class FieldStorage {
    private var fields = new ArrayBuffer[Field]
    private var exported = false
    private var isSorted = true

    private def beforeModify() {
      if (exported) {
        val t = new ArrayBuffer[Field]
        fields copyToBuffer t
        fields = t
        exported = false
      }
    }

    def +=(elem: Field) {
      beforeModify()
      if (isSorted && !fields.isEmpty && lt(elem, fields.last))
          isSorted = false
      fields += elem
    }

    def unsorted = fields.readOnly
    def sorted = {
      beforeModify()
      if (isSorted) {
        val t = new ArrayBuffer[Field]
        Sorting.stableSort(fields) copyToBuffer t
        fields = t
        isSorted = true
      }
      exported = true
      fields.readOnly
    }

    def lt(a: Field, b: Field) = a.name.compareTo(b.name) < 0
  }
  private val fields = new FieldStorage

  protected def sortedFields = fields.sorted

  def add(name: String, value: Int): MessageBuffer = add(IntField(name, value))
  def add(name: String, value: Long): MessageBuffer = add(LongField(name, value))

  def add(name: String, value: String): MessageBuffer =
    if (value != null)
      add(StringField(name, value))
    else
      this

  def add(name: String, value: Timestamp): MessageBuffer =
    if (value != null)
      add(TimestampField(name, value))
    else
      this

  def add(name: String, value: java.math.BigInteger): MessageBuffer =
    if (value != null)
      add(name, new BigInt(value))
    else
      this

  def add(name: String, value: BigInt): MessageBuffer =
    if (value != null)
      add(BigIntField(name, value))
    else
      this

  def add(name: String, value: java.math.BigDecimal): MessageBuffer =
    if (value != null)
      add(name, new BigDecimal(value))
    else
      this

  def add(name: String, value: BigDecimal): MessageBuffer =
    if (value != null)
      add(BigDecimalField(name, value))
    else
      this

  def add(name: String, value: Message): MessageBuffer =
    if (value != null && !value.isEmpty)
      add(MessageField(name, value))
    else
      this

  def add(field: Field) = this += field

  def +=(field: Field) = {
    fields += field
    this
  }

  def ++=(iter: Iterable[Field]) {
    for(f <- iter)
      add(f)
    this
  }

  def ++(iter: Iterable[Field]) = {
    val buffer = new MessageBuffer
    buffer ++= fields.unsorted
    buffer ++= iter
    buffer
  }

  def readOnly = Message(fields.sorted: _*) 

}
