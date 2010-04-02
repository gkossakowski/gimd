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

import com.google.gimd.{Field, Message}
import Parser._
import org.junit.Test
import org.junit.Assert._

final class ParserTestCase {

  @Test
  def emptyFile() {
    val msg = parse("")
    assertEquals(Message(), msg)
  }

  @Test{val expected = classOf[ParserException]}
  def LFasOnlyConent {
    parse(new java.io.StringReader("\n"))
  }

  @Test
  def oneFieldMessage {
    val msg = parse("fieldName value\n")
    assertEquals(Message(Field("fieldName", "value")), msg)
  }

  @Test
  def twoFieldMessage {
    val input = """|firstName John
                   |lastName Doe
                   |""".stripMargin
    val msg = parse(input)
    val expected = Message(Field("firstName", "John"), Field("lastName", "Doe"))
    assertEquals(expected, msg)
  }

  @Test{val expected = classOf[ParserException]}
  def fieldNameStartingWithNumber {
    parse(new java.io.StringReader("12fieldName 0\n"))
  }

  @Test{val expected = classOf[ParserException]}
  def fieldNameContainingNonLatinCharacter {
    parse(new java.io.StringReader("fieldżółty 0\n"))
  }

  @Test{val expected = classOf[ParserException]}
  def angleBracketsDoNotMatch {
    val input = """|firstName John
                   |nested1 <
                   |  nested2 <
                   |    nested3 <
                   |  >
                   |  nested4 <
                      >
                   |>
                   |
                   |""".stripMargin
    parse(input)
  }

  @Test
  def bareStringWithSpacesAsPrefix {
    val msg = parse("field   Prefix of two spaces\n")
    assertEquals(Message(Field("field", "  Prefix of two spaces")), msg)
  }

  @Test
  def bareStringWithNumberAsPrefix {
    val msg = parse("field 12.02 Prefix of two spaces\n")
    assertEquals(Message(Field("field", "12.02 Prefix of two spaces")), msg)
  }

  @Test
  def bareStringWithQuotedStringAsSuffix {
    val msg = parse("field Here comes quoted \"word\"\n")
    assertEquals(Message(Field("field", "Here comes quoted \"word\"")), msg)
  }

  @Test
  def bareStringWithBackslashAsPrefix {
    val msg = parse("field \\Backslash as prefix?\n")
    assertEquals(Message(Field("field", "\\Backslash as prefix?")), msg)
  }

  @Test{val expected = classOf[ParserException]}
  def bareStringWithUnescapedCharacter {
    parse("field Here comes tab:"+18.toChar+"\n")
  }

  @Test
  def emptyStringValue {
    val msg = parse("field \"\"\n")
    assertEquals(Message(Field("field", "")), msg)
  }

  @Test{val expected = classOf[ParserException]}
  def doesNotNeedQuotingString {
    parse("field \"This string doesn't need quoting!\"\n")
  }

  @Test
  def multiLineString {
    val input = """|lovePoem "
                   |  Hi my dear,
                   |  I haven't written to you because I had no ink!
                   |  You can imagine how I suffered from that situation. :(
                   |"
                   |""".stripMargin
    val expectedVal = """|Hi my dear,
                         |I haven't written to you because I had no ink!
                         |You can imagine how I suffered from that situation. :(""".stripMargin
    val msg = parse(input)
    assertEquals(Message(Field("lovePoem", expectedVal)), msg)
  }

  @Test
  def quotedSingleLineString {
    val msg = parse("field \"Here's quoted string with strange character: \\x0b\"\n")
    val expected = Message(Field("field", "Here's quoted string with strange character: "+11.toChar))
    assertEquals(expected, msg)
  }

  @Test{val expected = classOf[ParserException]}
  def wrongIndentionForMultiLineString {
    val input = """|level1 < "
                   |  level2 "
                   |    This is correctly intended
                   |  But this is not!
                   |  "
                   |>
                   |""".stripMargin
    parse(input)
  }

  @Test
  def indentionForMultiLineStringInNestedStructure {
    val input = """|level1 <
                   |  level2 <
                   |    msg "
                   |      This is correctly intended
                   |      And this is one too!
                   |    "
                   |  >
                   |>
                   |""".stripMargin
    val msg = parse(input)
    val expected =
    Message(Field("level1",
      Message(Field("level2",
        Message(Field("msg", "This is correctly intended\nAnd this is one too!"))
      ))
    ))
    assertEquals(expected, msg)
  }

  @Test
  def zeroValue {
    val msg = parse("field 0\n")
    assertEquals(Message(Field("field", 0)), msg)
  }

  @Test
  def decimalValueBetweenMinusOneAndZero {
    val msg = parse("field -0.334\n")
    assertEquals(Message(Field("field", BigDecimal("-0.334"))), msg)
  }

  @Test
  def decimalValueBetweenZeroAndOne {
    val msg = parse("field 0.012\n")
    assertEquals(Message(Field("field", BigDecimal("0.012"))), msg)
  }

  @Test
  def decimalValueWithLotsOfDigits {
    val msg = parse("field 34908547234897234623489.324893475783465345653648334\n")
    assertEquals(Message(Field("field", BigDecimal("34908547234897234623489.324893475783465345653648334"))), msg)
  }

  @Test
  def MinusZeroShouldBeString {
    val msg = parse("field -0\n")
    assertEquals(Message(Field("field", "-0")), msg)
  }

  @Test
  def wrongOrderOfTwoFields {
    val input = """|b 2
                   |a 1
                   |""".stripMargin
    assertInvalidFieldOrder(input)
  }

  @Test
  def wrongOrderOfFourFields {
    val input = """|b 1
                   |c 1
                   |a 1
                   |d 1
                   |""".stripMargin
    assertInvalidFieldOrder(input)
  }

  @Test
  def duplicatedFields {
    val input = """|a 1
                   |b 2
                   |b 2
                   |c 3
                   |""".stripMargin
    assertInvalidFieldOrder(input)
  }

  private def assertInvalidFieldOrder(input: String) {
    val expectedMsg = "Fields X, Y do not satisfy condition X < Y where"
    try {
      parse(input)
    } catch {
      //TODO this is a quick hack to make sure that exception we catch is carrying information about
      //TODO the error we are expecting. In a future it should be Parser that is changed to make
      //TODO such testing easier
      case e: ParserException => if (!e.getMessage.contains(expectedMsg)) throw e
    }
  }

  //TODO (1) Add tests for Int and Long specifically
  //TODO (2) Add tests for timestamps

}
