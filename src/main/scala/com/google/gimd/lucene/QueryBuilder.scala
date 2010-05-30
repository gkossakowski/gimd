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

import com.google.gimd.query.{ConstNode, FieldSpecOneNode, Query, Node}
import com.google.gimd.FieldSpecOne
import org.apache.lucene.index.Term
import org.apache.lucene.search._
import org.apache.lucene.search.{Query => LQuery}
import com.google.gimd.query.AllNodeOps.{Relational, Is}
import com.google.gimd.query.BooleanNodeOps.{Not, Or, And}

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

    def translateRelational(r: Relational[_], includeBound: Boolean): Option[TermRangeQuery] =
      r match {
        case Relational("<", FieldSpecOneNode(fs), ConstNode(v)) =>
          //FIXME: See the comment in translateIs above
          Some(new TermRangeQuery(fs.name, null, v.toString, false, includeBound))
      }

    def translateRelationalOrIs(r: Relational[_], i: Is[_]): Option[TermRangeQuery] ={
      def matchIs(i: Is[_], left: FieldSpecOneNode[_,_], right: ConstNode[_]) = i match {
        case Is(l, r) if (l == left && r == right) => true
        case _ => false
      }
      r match {
        case Relational("<", left: FieldSpecOneNode[_,_], right: ConstNode[_])
          if matchIs(i, left, right) => translateRelational(r, true)
      }
    }

    node match {
      case Or(r : Relational[_], i : Is[_]) => translateRelationalOrIs(r, i)
      case Or(i : Is[_], r : Relational[_]) => translateRelationalOrIs(r, i)
      case Not(r: Relational[_]) => translateRelational(r, false) map { x =>
        //'!<' is the same as '>='
        new TermRangeQuery(x.getField, x.getUpperTerm, null, true, false)
      }
      case And(Not(r: Relational[_]), Not(i: Is[_])) => translateRelationalOrIs(r, i) map { x =>
        //'!< && !=' is the same as '>'
        new TermRangeQuery(x.getField, x.getUpperTerm, null, false, false)
      }
      case r: Relational[_] => translateRelational(r, false)
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
