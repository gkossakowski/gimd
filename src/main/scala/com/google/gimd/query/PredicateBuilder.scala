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

import com.google.gimd.FieldSpecOne
import reflect.Manifest
import com.google.gimd.query.AllNodeOps.{Relational, Is}
import com.google.gimd.query.BooleanNodeOps.{Not, Or, And}

/**
 * PredicateBuilder is an object that transforms Query AST into Predicate instance.
 *
 * The transformation is performed by mapping AST nodes to simple combinator closures.
 *
 * It takes instance of Manifest because Predicate requires it.
 */
object PredicateBuilder {

  //would love if PredicateBuilder extended (Query[_,_] => Predicate[_]) but manifests make it
  //impossible as it seems there is no type conveying the fact functions takes implicit parameters
  def apply[T:Manifest](query: Query[T,_]): Predicate[T] = translateNode(query.node)

  private def translateNode[T](node: Node[_])(implicit m: Manifest[T]): Predicate[T] = {
    def extractConverter[F](n: FieldSpecOneNode[_,F]): T => F =
      if (m == n.manifest)
        n.fieldSpecOne.f2.asInstanceOf[T => F]
      else
        throw new IllegalArgumentException(("Received FieldSpecOneNode with wrong manifest. " +
                  "Expected %s but received %s").format(m, n.manifest))

    def translateIs[F,X](f: T => F, v: F): Predicate[T] = Predicate(f(_) == v)

    def translateAnd(left: Predicate[T], right: Predicate[T]): Predicate[T] =
      Predicate(x => left.isMatch(x) && right.isMatch(x))

    def translateOr(left: Predicate[T], right: Predicate[T]): Predicate[T] =
      Predicate(x => left.isMatch(x) || right.isMatch(x))

    def negatePredicate(p: Predicate[T]): Predicate[T] = Predicate(!p.isMatch(_))

    node match {
      case Is(n @ FieldSpecOneNode(_), ConstNode(v)) => translateIs(extractConverter(n), v)
      case Is(ConstNode(v), n @ FieldSpecOneNode(_)) => translateIs(extractConverter(n), v)
      case And(left, right) => translateAnd(translateNode(left), translateNode(right))
      case Or(left, right) => translateOr(translateNode(left), translateNode(right))
      case ConstNode(true) => Predicate(_ => true)
      case Not(left) => negatePredicate(translateNode(left))
      case Relational("<", n @ FieldSpecOneNode(_), ConstNode(v)) => {
        val f = extractConverter(n)
        Predicate(x => n.ordering.lt(f(x), v))
      }
      case Relational("<", ConstNode(v), n @ FieldSpecOneNode(_)) => {
        val f = extractConverter(n)
        Predicate(x => n.ordering.lt(v, f(x)))
      }
      case x => error(x + "anything else is not supported at the moment")
    }
  }

}
