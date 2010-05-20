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

import org.junit.runner.RunWith
import org.scalacheck.{Prop, Properties}
import com.google.gimd.testingutil.JUnit4Runner
import com.google.gimd.Message

@RunWith(classOf[JUnit4Runner])
class FormatterParserMessageTestCase extends Properties("formatAndParseMessage") {
  import TreeMessageGenerator.arbMessage
  import MessageShrinker.shrinkMessage

  property("formatAndParseAgainMessage") =
          Prop.forAll((m: Message) => m == Parser.parse(Formatter.format(m)))
}
