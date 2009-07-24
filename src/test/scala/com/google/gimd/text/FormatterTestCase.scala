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

import org.junit.Test
import org.junit.Assert.assertEquals
import Formatter._

final class FormatterTestCase {

  @Test
  def emptyMessage {
    assertEquals("", format(Message()))
  }

  @Test
  def oneFieldMessage {
    assertEquals("fieldName value\n", format(Message(Field("fieldName", "value"))))
  }

  @Test
  def twoFieldMessage {
    val output = format(Message(Field("firstName", "John"), Field("lastName", "Doe")))
    val expected = """|firstName John
                      |lastName Doe
                      |""".stripMargin
    assertEquals(expected, output)
  }

  @Test
  def bareStringWithSpacesAsPrefix {
    val expected = "field   Prefix of two spaces\n"
    val output = format(Message(Field("field", "  Prefix of two spaces")))
    assertEquals(expected, output)
  }

  @Test
  def bareStringWithNumberAsPrefix {
    val output = format(Message(Field("field", "12.02 Prefix of two spaces")))
    val expected = "field 12.02 Prefix of two spaces\n"
    assertEquals(expected, output)
  }

  @Test
  def bareStringWithQuotedStringAsSuffix {
    val output = format(Message(Field("field", "Here comes quoted \"word\"")))
    val expected = "field Here comes quoted \"word\"\n"
    assertEquals(expected, output)
  }

  @Test
  def bareStringWithBackslashAsPrefix {
    val output = format(Message(Field("field", "\\Backslash as prefix?")))
    val expected = "field \\Backslash as prefix?\n"
    assertEquals(expected, output)
  }

  @Test
  def emptyStringValue {
    val output = format(Message(Field("field", "")))
    val expected = "field \"\"\n"
    assertEquals(expected, output)
  }

  @Test
  def multiLineString {
    val expected = """|lovePoem "
                      |  Hi my dear,
                      |  I haven't written to you because I had no ink!
                      |  You can imagine how I suffered from that situation. :(
                      |"
                      |""".stripMargin
    val fieldValue = """|Hi my dear,
                        |I haven't written to you because I had no ink!
                        |You can imagine how I suffered from that situation. :(""".stripMargin
    val output = format(Message(Field("lovePoem", fieldValue)))
    assertEquals(expected, output)
  }

  @Test
  def quotedSingleLineStringWithEscapes {
    val expected = "field \"Here's quoted string with strange character: \\x0b\"\n"
    val output = format(Message(Field("field", "Here's quoted string with strange character: "+11.toChar)))
    assertEquals(expected, output)
  }

  @Test
  def indentionForMultiLineStringInNestedStructure {
    val expected = """|level1 <
                      |  level2 <
                      |    msg "
                      |      This is correctly intended
                      |      And this is one too!
                      |    "
                      |  >
                      |>
                      |""".stripMargin
    val msg =
    Message(Field("level1",
      Message(Field("level2",
        Message(Field("msg", "This is correctly intended\nAnd this is one too!"))
      ))
    ))
    val output = format(msg)
    assertEquals(expected, output)
  }

  @Test
  def zeroValue {
    val output = format(Message(Field("field", 0)))
    val expected = "field 0\n"
    assertEquals(expected, output)
  }

  @Test
  def decimalValueBetweenMinusOneAndZero {
    val output = format(Message(Field("field", BigDecimal("-0.334"))))
    val expected = "field -0.334\n"
    assertEquals(expected, output)
  }

  @Test
  def decimalValueBetweenZeroAndOne {
    val output = format(Message(Field("field", BigDecimal("0.012"))))
    val expected = "field 0.012\n"
    assertEquals(expected, expected)
  }

  @Test
  def decimalValueWithLotsOfDigits {
    val output = format(Message(Field("field", BigDecimal("34908547234897234623489.324893475783465345653648334"))))
    val expected = "field 34908547234897234623489.324893475783465345653648334\n"
    assertEquals(expected, output)
  }

}
