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
package com.google.gimd

import util.Random
import TestTree._

/**
 * Class used to generate simple random tree of Nodes.
 *
 * The companion object defines UserTypes for these two kinds of Nodes.
 */
class RandomTreeGenerator(val rnd: Random) {

  protected val MAX_CHILD_NODES = 3
  protected val MAX_DISTINCT_NAMES = 10000
  protected val MAX_NAME_LENGTH = 20
  protected val MAX_DISTINCT_IDS = 10000
  protected val MAX_ID = 1000000
  private val MAX_DEPTH = 5

  lazy val names = for (_ <- (1 to MAX_DISTINCT_NAMES)) yield
    nextASCIIString(rnd, 1 + rnd.nextInt(MAX_NAME_LENGTH))
  lazy val ids = for (_ <- (1 to MAX_DISTINCT_IDS)) yield rnd.nextInt(MAX_ID)

  def generate(maxNumberOfNodes: Int): (List[Node1], List[Node2]) = {
    val nodes1 = for (_ <- (1 to maxNumberOfNodes)) yield generateNode1(MAX_DEPTH)
    val nodes2 = for (_ <- (1 to maxNumberOfNodes)) yield generateNode2(MAX_DEPTH)
    (nodes1.toList, nodes2.toList)
  }

  def generateNode1(depth: Int = MAX_DEPTH): Node1 = {
    val name = names(rnd.nextInt(names.size))
    val nodes1 =
      if (depth > 0)
        for (_ <- (0 to (rnd.nextInt(MAX_CHILD_NODES))).toList) yield
          generateNode1(rnd.nextInt(depth))
      else Nil

    val nodes2 =
      if (depth > 0)
        for (_ <- (0 to (rnd.nextInt(MAX_CHILD_NODES))).toList) yield
          generateNode2(rnd.nextInt(depth))
      else Nil
    Node1(name, nodes1.toList, nodes2.toList)
  }

  def generateNode2(depth: Int = MAX_DEPTH): Node2 = {
    val id = ids(rnd.nextInt(ids.size))
    val nodes1 =
      if (depth > 0)
        for (_ <- (0 to (rnd.nextInt(MAX_CHILD_NODES))).toList) yield
          generateNode1(rnd.nextInt(depth))
      else Nil

    val nodes2 =
      if (depth > 0)
        for (_ <- (0 to (rnd.nextInt(MAX_CHILD_NODES))).toList) yield
          generateNode2(rnd.nextInt(depth))
      else Nil
    Node2(id, nodes1.toList, nodes2.toList)
  }

  /** Returns a pseudorandomly generated String drawing upon
    *  only ASCII characters between 33 and 126.
    */
  private def nextASCIIString(rnd: Random, length: Int) = {
    val urn = (List.range(48, 57) ++ List.range(65, 90) ++ List.range(97, 122)).toArray
    def nextDigit = urn(rnd.nextInt(urn.size))

    val bytes = for (_ <- 0 to length) yield nextDigit.toByte

    new String(bytes.toArray, "ASCII")
  }
}
