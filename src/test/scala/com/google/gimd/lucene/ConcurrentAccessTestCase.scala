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

import com.google.gimd._
import jgit.{JGitDatabase, AbstractJGitTestCase}
import file.FileType
import UserType._
import query.Query._
import org.junit.Test
import actors.Futures
import org.junit.Assert._

class ConcurrentAccessTestCase extends AbstractJGitTestCase with LuceneTestCase {

  override val deleteRepository = true

  case class Counter(name: String, value: Int)

  object CounterType extends UserType[Counter] {
    val name = FieldSpecOne("name", StringField, _.name)
    val value = FieldSpecOne("value", IntField, _.value)
    def fields = name :: value :: Nil
    def toUserObject(m: Message): Counter = Counter(name(m), value(m))
  }

  object CounterFileType extends FileType[Counter] {
    val pathPrefix = Some("counters/")
    val pathSuffix = None
    val userType = CounterType
    def name(m: Message) = userType.name(m)
  }

  val fileTypes = CounterFileType :: Nil

  private def concurrentModifications(db: com.google.gimd.Database) {
    val N = 20
    db.modify { _ =>
      val counters = (1 to N) map(x => Counter("counter"+x, 0))
      counters.foldLeft(modification.DatabaseModification.empty) {
        case (m, x) => m.insertFile(CounterFileType, x)
      }
    }
    val futures = for (i <- 1 to N) yield Futures.future {
      db.modify { s =>
        val q = CounterType.query where { _.name === ("counter"+i) }
        val (handle, counter) = s.query(CounterFileType, q).toList.head
        val newCounter = Counter(counter.name, counter.value+1)
        modification.DatabaseModification.empty.modify(handle, newCounter)
      }
    }
    Futures.awaitAll(10000, futures: _*)
    assertTrue(db.query(CounterFileType, CounterType.query).forall(_.value == 1))
  }

  @Test
  def concurrentModificationsWithoutLucene = withDb { db =>
    concurrentModifications(db)
  }

  @Test
  def concurrentModificationsWithLucene = withLuceneDb { db =>
    concurrentModifications(db)
  }


}
