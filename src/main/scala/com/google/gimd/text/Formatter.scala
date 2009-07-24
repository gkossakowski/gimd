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

package com.google.gimd.text

import com.google.gimd._

object Formatter {
  def format(m: Message) = new Formatter().format(m).toString
  def format(f: Field) = new Formatter().format(f).toString
  def valueString(f: Field) = new Formatter().formatValue(f).toString
  def valueString(t: Timestamp) = new Formatter().formatValue(t).toString
}

class Formatter {
  private val buf = new StringBuilder()
  private val timestampUTC = TextLanguageRules.timestampFormat(true)
  private val timestampAny = TextLanguageRules.timestampFormat(false)

  override def toString = buf.toString

  def format(m: Message): Formatter = {
    message(0, m)
    this
  }

  def format(f: Field): Formatter = {
    field(0, f)
    this
  }

  def formatValue(f: Field): Formatter = {
    value(0, f)
    this
  }

  def formatValue(ts: Timestamp): Formatter = {
    buf.append(formatter(ts).format(ts))
    this
  }

  private def formatter(ts: Timestamp) =
    if (ts.offset == 0)
      timestampUTC
    else {
      timestampAny.setTimeZone(ts.timeZone)
      timestampAny
    }

  private def message(level: Int, m: Message) = {
    for(f <- m) {
      field(level, f)
      buf.append('\n')
    }
  }

  private def field(level: Int, f: Field) = {
    indent(level)
    buf.append(f.name)
    buf.append(' ')
    value(level, f)
  }

  private def indent(level: Int): Unit  =
    if (level > 0) {
      buf.append("  ")
      indent(level - 1)
    }

  private def value(level: Int, f: Field): Unit = f match {
    case m: MessageField => nested(level, m.value)
    case s: StringField  => string(level, s.value)
    case b: BigDecimalField => buf.append(format(b.value.bigDecimal))
    case n: NumberField  => buf.append(n.toNumber.toString)
    case t: TimestampField => formatValue(t.value)
  }

  private def nested(level: Int, m: Message) = {
    buf.append("<\n")
    message(level + 1, m)
    indent(level)
    buf.append('>')
  }

  private def string(level: Int, s: String) =
    if (TextLanguageRules.isQuotingRequired(s))
      quoteString(level, s)
    else
      buf.append(s)

  private def quoteString(level: Int, s: String) = {
    buf.append('"')
    val isMultiLine = s.contains('\n')
    if (isMultiLine) {
      buf.append('\n')
      indent(level+1)
    }

    for(c <- s) {
      c match {
        case '\\' => buf.append("\\\\")
        case '\"' => buf.append("\\\"")
        case '\n' => { buf.append(c); indent(level+1) }
        case _ =>
          if (TextLanguageRules.isNeedHexEscape(c))
            hexEscape(c)
          else
            buf.append(c)
      }
    }

    if (isMultiLine) {
      buf.append('\n')
      indent(level)
    }
    buf.append('"')
  }

  private def hexEscape(ch: Char) = {
    buf.append("\\x")
    buf.append(toHex(ch >>> 4))
    buf.append(toHex(ch & 0xf))
  }

  private def toHex(v: Int): Char =
    if (v < 10)
      ('0' + v).toChar
    else
      ('a' + (v - 10)).toChar

  private def format(b: java.math.BigDecimal): String =
    if (b.scale < 0)
      format(b.movePointRight(b.scale abs))
    else if (b.scale == 0)
      b.toString
    else
      format2(b.stripTrailingZeros)
  private def format2(b: java.math.BigDecimal): String =
    if (b.scale < 0)
      format(b)
    else
      b.toString
}
