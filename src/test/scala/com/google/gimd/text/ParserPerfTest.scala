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

import org.junit.Test
import com.google.gimd.{PerfTest, MessageField, Message}

class ParserPerfTest extends PerfTest {

  private def generateMessage(rnd: util.Random, minRank: Int): Option[Message] = {
    import com.google.gimd.RandomTreeGenerator
    import com.google.gimd.TestTree.Node1Type
    val gen = new RandomTreeGenerator(rnd)
    val msgs = Iterator.continually(Node1Type.toMessage(gen.generateNode1(gen.names, gen.ids)))
    msgs.find(messageRank(_) > minRank)
  }

  @Test
  def parse {
    val minRank = 100
    val msg = generateMessage(new util.Random(101), minRank) match {
      case Some(msg) => msg
      case None => error("Failed to find message with rank above %d".format(minRank))
    }
    val text = Formatter.format(msg)
    val rank = messageRank(msg)
    println("Rank for tested message is %d and the size of serialized message is %d bytes".
            format(rank, text.size))
    def parse = Parser.parse(text); ()
    println(formatInfo("parse", measureTime(1000, parse _)))
  }

  private def messageRank(m: Message): Int = {
    val nestedMsgs = m.collect { case x: MessageField => x }
    val simpleFields = m -- nestedMsgs
    1 + simpleFields.size + nestedMsgs.map((x: MessageField) => messageRank(x.value)).sum
  }

}
