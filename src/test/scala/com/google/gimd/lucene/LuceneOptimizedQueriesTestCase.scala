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
package com.google.gimd.lucene

import com.google.gimd.jgit.{JGitDatabase, AbstractJGitTestCase}
import com.google.gimd.modification.DatabaseModification
import com.google.gimd.TestTree._
import org.junit.Test
import org.junit.Assert._

class LuceneOptimizedQueriesTestCase extends AbstractJGitTestCase {

  private val fileTypes = Node1FileType :: Node2FileType :: Nil

  private def createDatabase =
    new JGitDatabase(fileTypes, masterBranch) with LuceneOptimizedDatabase

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
  def testQueryForMissingObject {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q = Node1Type.query where { _.name === "non-existing" }
    assertEquals(0, db.query(Node1FileType, q).size)
  }

  @Test
  def eqIntQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q = Node2Type.query where { _.id === 2 }
    assertEquals(1, db.query(Node2FileType, q).size)
  }

  @Test
  def nameLessThanQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q = Node1Type.query where { _.name < "node4" }
    assertEquals(3, db.query(Node1FileType, q).size)
  }

  @Test
  def nameMoreThanOrEqQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q = Node1Type.query where { _.name >= "node2" }
    assertEquals(4, db.query(Node1FileType, q).size)
  }

  @Test
  def idLessThanQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q = Node2Type.query where { _.id < 5 }
    assertEquals(4, db.query(Node2FileType, q).size)
  }

  @Test
  def idMoreThanOrEqQuery {
    val db = createDatabase
    createNodes(db)
    import com.google.gimd.query.Query._
    val q = Node2Type.query where { _.id >= 2 }
    assertEquals(4, db.query(Node2FileType, q).size)
  }

  @Test
  def fullAndIncrementalIndexing {
    val db = createDatabase
    db.modify { s =>
      //this query forces empty index creation
      if (s.query(Node1FileType, Node1Type.query).isEmpty) {
        println("adding")
        DatabaseModification.empty.insertFile(Node1FileType, Node1("node", Nil, Nil))
      } else DatabaseModification.empty
    }
    //this query executes incremental indexing
    assertEquals(1, db.query(Node1FileType, Node1Type.query).size)
  }

}
