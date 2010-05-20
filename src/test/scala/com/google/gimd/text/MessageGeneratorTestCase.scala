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
import com.google.gimd.{MessageField, Message}
import org.junit.Assert._

class MessageGeneratorTestCase {

  @Test
  def checkRatingOfTreeMessageGenerator {
    //calculates simple rating of a complexity of a Message
    def rate(m: Message, depth: Int): Int = {
      val childMsgs = m.toList.flatMap {
        case MessageField(_, msg) => List(msg)
        case _ => Nil
      }
      depth * m.toList.size + childMsgs.foldLeft(0)(_ + rate(_, depth+1))
    }

    val ratings = for (_ <- 1 to 1000) yield {
      val msg = TreeMessageGenerator.arbMessage.arbitrary.sample.get
      rate(msg, 1)
    }
    val avg = (ratings.foldLeft(0)(_+_): Double) / ratings.size
    val variance = ratings.map(x => (avg - x)*(avg - x)).foldLeft(0: Double)(_+_)
    assertTrue("Average rating for complexity of generated messages should be above 20", avg > 20)
    assertTrue("We expect that variance should be really high", variance > 10e4)
  }

}
