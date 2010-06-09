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
package com.google.gimd.jgit

import com.google.gimd._
import lucene.{LuceneTestCase, LuceneOptimizedDatabase}
import modification.DatabaseModification
import scala.util.Random
import org.junit.Test
import com.google.gimd.TestTree._

/**
 * Simple performance tests that allows one to see the difference in execution time between
 * Lucene-optimized queries and full scan queries.
 */
class LuceneOptimizedDatabasePerfTest extends AbstractJGitTestCase with LuceneTestCase
        with PerfTest {


  val fileTypes: List[FileType[_]] = List(CounterFileType, Node1FileType, Node2FileType)

  private def createNodes(rnd: Random): DatabaseModification = {
    val MAX_NUMBER_OF_NODES = 100
    val (nodes1, nodes2) = (new RandomTreeGenerator(rnd)).generate(MAX_NUMBER_OF_NODES)
    println("Generated " + (nodes1.size + nodes2.size) + " nodes")
    val mod1 = nodes1.foldLeft(DatabaseModification.empty) {
        case (m, node1) => m.insertFile(Node1FileType, node1)
      }
    val mod2 = nodes2.foldLeft(mod1) {
        case (m, node2) => m.insertFile(Node2FileType, node2)
      }
    mod2
  }

  private def generateNodes(db: JGitDatabase) {
    val rnd = new Random(104)
    val avgVar = measureTime(1, () => db.modify(_ => createNodes(rnd)))
    println(formatInfo("generateNodes", avgVar))
  }

  @Test
  def measureEqQueryTime = withLuceneDb { db =>
    println("Measuring 'equality' query time.")
    import com.google.gimd.query.Query._
    val q1 = Node1Type.query where { _.name === "GqfDGUe" }
    val q2 = Node2Type.query where { _.id === 114528 }
    val a1 = () => {
      db.latestSnapshot.query(Node1FileType, q1).toList
      db.latestSnapshot.query(Node2FileType, q2).toList
      ()
    }
    val a2 = () => {
      db.latestSnapshot.query(Node1FileType, q1.predicate).toList
      db.latestSnapshot.query(Node2FileType, q2.predicate).toList
      ()
    }

    println("warming up")
    for (_ <- 1 to 5) a1()
    for (_ <- 1 to 5) a2()

    val N = 100

    println("Measuring with Lucene enabled")
    println(formatInfo("query with Lucene enabled", measureTime(N, a1)))

    println("Measuring without Lucene enabled")
    println(formatInfo("query without Lucene enabled", measureTime(N, a2)))
  }

  @Test
  def measureLtQueryTime = withLuceneDb { db =>
    println("Measuring 'less than' query time.")
    generateNodes(db)
    import com.google.gimd.query.Query._
    val q1 = Node1Type.query where { _.name < "aaa" }
    val q2 = Node2Type.query where { _.id < 100 }
    val a1 = () => {
      db.latestSnapshot.query(Node1FileType, q1).toList
      db.latestSnapshot.query(Node2FileType, q2).toList
      ()
    }
    val a2 = () => {
      db.latestSnapshot.query(Node1FileType, q1.predicate).toList
      db.latestSnapshot.query(Node2FileType, q2.predicate).toList
      ()
    }

    println("warming up")
    for (_ <- 1 to 5) a1()
    for (_ <- 1 to 5) a2()

    val N = 100

    println("Measuring with Lucene enabled")
    println(formatInfo("query with Lucene enabled", measureTime(N, a1)))

    println("Measuring without Lucene enabled")
    println(formatInfo("query without Lucene enabled", measureTime(N, a2)))
  }

}
