// Copyright (C) 2010 The Android Open Source Project
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

import org.scalacheck.Properties
import com.google.gimd.testingutil.JUnit4Runner
import org.junit.runner.RunWith

@RunWith(classOf[JUnit4Runner])
class FormatterParserFieldTestCase extends Properties("formatAndParseField") {
  import org.scalacheck.Prop
  import com.google.gimd.{Field, IntField, StringField, Message}
  import TreeMessageGenerator.{arbIntField, arbStringField}
  import MessageShrinker.shrinkField

  def formatToMessage(x: Field) = Formatter.format(x) + "\n"

  property("formatAndParseAgainIntField") =
          Prop.forAll((x: IntField) => Message(x) == Parser.parse(formatToMessage(x)))

  property("formatAndParseAgainStringField") =
          Prop.forAll((x: StringField) => Message(x) == Parser.parse(formatToMessage(x)))

  //TODO add properties for rest of field types

}
