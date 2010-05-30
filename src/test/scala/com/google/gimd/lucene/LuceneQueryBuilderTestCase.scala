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

import com.google.gimd.TestTree.Node1Type
import com.google.gimd.query.Query._
import org.junit.Test
import org.junit.Assert._

class LuceneQueryBuilderTestCase {

  @Test
  def lessThanQuery {
    val q = Node1Type.query where { _.name < "node2" }
    val lq = QueryBuilder(q)
    assertTrue("QueryBuilder should handle < operator.", lq.isDefined)
  }

  @Test
  def lessThanOrEqQuery {
    val q = Node1Type.query where { _.name <= "node2" }
    val lq = QueryBuilder(q)
    println(lq)
    assertTrue("QueryBuilder should handle <= operator.", lq.isDefined)
  }

  @Test
  def greaterThanOrEqQuery {
    val q = Node1Type.query where { _.name >= "node2" }
    val lq = QueryBuilder(q)
    println(lq)
    assertTrue("QueryBuilder should handle <= operator.", lq.isDefined)
  }

  @Test
  def greaterThanQuery {
    val q = Node1Type.query where { _.name > "node2" }
    val lq = QueryBuilder(q)
    println("> case " + lq)
    assertTrue("QueryBuilder should handle > operator.", lq.isDefined)
  }

}
