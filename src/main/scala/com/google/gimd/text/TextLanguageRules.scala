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

object TextLanguageRules {
  /**
   * True if the character should use a \xNN format escape code.
   * 
   * All characters less than 0x20 (aka ASCII space) should use a hex escape
   * code, unless they are one of the following common printables used in that
   * space: \t (tab), \n (LF), \r (CR).
   */
  def isNeedHexEscape(ch: Char) =
    if (ch < 0x20)
      ch match {
        case 0x09 /* \t */ => false
        case 0x0A /* \n */ => false
        case 0x0D /* \r */ => false
        case _ => true
      }
    else
      false

  /**
   * True if the string must appear inside of double quotes.
   * 
   * Strings should only be quoted if one of the following conditions is met:
   * 
   * 1) Starts with '<' or '"'.  Such a string could be confused for the start
   * of a nested message, or a quoted string.  To start a string with one of
   * these characters the quoted format must be used.
   * 
   * 2) String contains '\n' (LF).  Such a string will span multiple lines, and
   * the quoted format must be used to span over the line breaks that would
   * otherwise be assumed to be field terminators.
   * 
   * 3) String contains a control character less than code point 0x20
   * (ASCII space) that is not \t (tab), \n (LF), \r (CR).  Some parser
   * implementations and/or file handling tools might mistake a control
   * character such as \x03 (end of text) as a different meaning than as
   * a literal value in the field.  Escaping the character as a hex format
   * inside of a quoted string ensures this will not happen.
   *
   * 4) Can be parsed as numeric or Timestamp value. Then such a string value must be quoted so
   * it can be really interpreted as a string field and not a numeric or Timestamp one.  
   */
  def isQuotingRequired(s: String) =
    (  s.isEmpty
    || s(0) == '<'
    || s(0) == '"'
    || s.exists(isQuoteRequired _)
    || isNumeric(s)
    || isTimestamp(s)
    )

  private def isQuoteRequired(ch: Char) =
    (   ch == '\n'
    || isNeedHexEscape(ch)
    )

  private def isNumeric(x: String): Boolean = {
    val parser = new Parser
    parser.numeric(new scala.util.parsing.input.CharSequenceReader(x + "\n")) match {
      case _: parser.Success[_] => true
      case _ => false
    }
  }

  private def isTimestamp(x: String): Boolean = {
    val parser = new Parser
    parser.timestamp(new scala.util.parsing.input.CharSequenceReader(x + "\n")) match {
      case _: parser.Success[_] => true
      case _ => false
    }
  }

  /**
   * Construct a new DateFormat for the ISO 8601 / RFC 3339 format.
   * @see http://www.w3.org/TR/NOTE-datetime
   */
  def timestampFormat(inUTC: Boolean): java.text.DateFormat = {
    val fmt = new java.text.SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss.SSS" + (if (inUTC) "'Z'" else "ZZZZZ")
    )
    if (inUTC)
      fmt.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    fmt.setLenient(false)
    fmt
  }
}
