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

import com.google.gimd.query.AllNodeOps.Is
import com.google.gimd.query.{ConstNode, FieldSpecOneNode, Query, Node}
import com.google.gimd.FieldSpecOne
import org.apache.lucene.index.Term
import com.google.gimd.query.BooleanNodeOps.{Or, And}
import org.apache.lucene.search._
import org.apache.lucene.search.{Query => LQuery}

/**
 * QueryBuilder which builds Lucene representation of a Gimd's query if possible.
 */
object QueryBuilder extends (Query[_,_] => Option[LQuery]) {

  /**
   * Builds Lucene's Query representation for a given Gimd query.
   *
   * The return type is optional because not every Gimd query can be represented by Lucene query
   * which will be efficient to execute (that's the whole purpose of Lucene queries).
   *
   * The most obvious and extreme example is that Gimd query can include arbitrary predicate
   * T => Boolean which is just a closure that one cannot translate but only apply on a given
   * instance of type T. 
   */
  def apply(query: Query[_,_]): Option[LQuery] = translateNode(query.node)

  private def translateNode(node: Node[_]): Option[LQuery] = {
    def translateIs[T](fs: FieldSpecOne[_,_], v: T): LQuery =
      //FIXME: Calling toString is probably not the best idea as we would
      //like to have different way to serialize value to string (e.g. in Date case)
      new TermQuery(new Term(fs.name, v.toString))

    def translateAnd(left: LQuery, right: LQuery): LQuery = {
      val q = new BooleanQuery()
      q.add(left, BooleanClause.Occur.MUST)
      q.add(right, BooleanClause.Occur.MUST)
      q
    }

    def translateOr(left: LQuery, right: LQuery): LQuery = {
      val q = new BooleanQuery()
      //this is the way to say that at least one of BooleanClauses should match which is really
      //expressing the Or combinator
      q.setMinimumNumberShouldMatch(1)
      q.add(left, BooleanClause.Occur.SHOULD)
      q.add(right, BooleanClause.Occur.SHOULD)
      q
    }

    node match {
      case Is(FieldSpecOneNode(fs), ConstNode(v)) => Some(translateIs(fs, v))
      case Is(ConstNode(v), FieldSpecOneNode(fs)) => Some(translateIs(fs, v))
      case And(left, right) => for {
        qLeft <- translateNode(left)
        qRight <- translateNode(right)
      } yield translateAnd(qLeft, qRight)
      case Or(left, right) => for {
        qLeft <- translateNode(left)
        qRight <- translateNode(right)
      } yield translateOr(qLeft, qRight)
      case ConstNode(true) => Some(new MatchAllDocsQuery)
      case _ => None //anything else is not supported at the moment
    }
  } 

}
