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
package com.google.gimd.query

import com.google.gimd.jgit.{JGitDatabase, AbstractJGitTestCase}
import com.google.gimd.TestTree._
import com.google.gimd.modification.DatabaseModification
import org.junit.Test
import org.junit.Assert._

class QueryTestCase extends AbstractJGitTestCase {
  private val fileTypes = Node1FileType :: Node2FileType :: Nil

  private def createDatabase = new JGitDatabase(fileTypes, masterBranch)

  private def createNodes(db: JGitDatabase): Unit = {
    val MAX_NUMBER_OF_NODES = 5
    val nodes1 = for (i <- 1 to MAX_NUMBER_OF_NODES) yield Node1("node" + i, Nil, Nil)
    val nodes2 = for (i <- 1 to MAX_NUMBER_OF_NODES) yield Node2(i, Nil, Nil)
    val mod1 = nodes1.foldLeft(DatabaseModification.empty) {
        case (m, node1) => m.insertFile(Node1FileType, node1)
      }
    val mod2 = nodes2.foldLeft(mod1) {
        case (m, node2) => m.insertFile(Node2FileType, node2)
      }
    mod2
    db.modify(_ => mod2)
  }

  @Test
  def testGreaterThanQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q1 = Node1Type.query where { _.name > "node" }
    val q2 = Node2Type.query where { _.id > 1 }
    assertEquals(5, db.query(Node1FileType, q1).size)
    assertEquals(5-1, db.query(Node2FileType, q2).size)
  }

  @Test
  def testLessThanOrEqQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q1 = Node1Type.query where { _.name <= "node2" }
    val q2 = Node2Type.query where {  _.id <= 3 }
    assertEquals(2, db.query(Node1FileType, q1).size)
    assertEquals(3, db.query(Node2FileType, q2).size)
  }
}
