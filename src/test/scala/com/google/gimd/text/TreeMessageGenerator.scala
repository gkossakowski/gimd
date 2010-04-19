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

import org.scalacheck._
import Arbitrary.arbitrary
import Gen._
import com.google.gimd.{MessageField, IntField, StringField, Message}

/**
 * Simple generator which generates nested messages of following forms.
 * child1:
 * <pre>
 *   &lt;
 *     name some name
 *     child1 ...
 *     child2 ...
 *   &gt;
 * </pre>
 *
 * child2:
 * <pre>
 *   &lt;
 *     id 123
 *     child1 ...
 *     child2 ... 
 *   &gt;
 * </pre> 
 */
object TreeMessageGenerator {

  val MAX_BRANCH = 5

  def alphaNumStr: Gen[String] = for(cs <- listOf(Gen.alphaNumChar)) yield cs.mkString

  // it should be choose(0, Integer.MAX_VALUE) but then we get stackoverflow due to some bug in
  // ScalaCheck
  def id: Gen[Int] = arbitrary[Int]

  def genIntField: Gen[IntField] = for (i <- id) yield IntField("name", i)

  def genStringField: Gen[StringField] = for (n <- alphaNumStr) yield StringField("name", n)

  def genChildMessageField(depth: Int) = ( genChild1(depth) map (MessageField("child1", _)) ) |
          ( genChild2(depth) map (MessageField("child2", _)) )

  def genChild1(depth: Int): Gen[Message] =
    for (x <- genStringField; children <- frequency(
            (5+5*depth,Nil),
            (1,listOfN(MAX_BRANCH, genChildMessageField(depth)))
          )
    ) yield Message(x :: children)

  def genChild2(depth: Int): Gen[Message] =
    for (x <- genIntField; children <- frequency(
            (5+5*depth,Nil),
            (1,listOfN(MAX_BRANCH, genChildMessageField(depth)))
          )
    ) yield Message(x :: children)

  implicit val arbMessage = Arbitrary(genChild1(0) | genChild2(0))
  implicit val arbIntField = Arbitrary(genIntField)
  implicit val arbStringField = Arbitrary(genStringField)

}
