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
import com.google.gimd.query.AllNodeOps.Is
import com.google.gimd.query.BooleanNodeOps.{Or, And}
import reflect.Manifest

/**
 * PredicateBuilder is an object that transforms Query AST into Predicate instance.
 *
 * The transformation is performed by mapping AST nodes to simple combinator closures.
 *
 * It takes instance of Manifest because Predicate requires it.
 */
object PredicateBuilder {

  def predicate[T](query: Query[T,_])(implicit m: Manifest[T]): Predicate[T] =
    translateNode(query.node)

  private def translateNode[T](node: Node[_])(implicit m: Manifest[T]): Predicate[T] = {
    def translateIs[F](fs: FieldSpecOne[T,F], v: F): Predicate[T] =
      Predicate(fs.f2(_) == v)

    def translateAnd(left: Predicate[T], right: Predicate[T]): Predicate[T] =
      Predicate(x => left.isMatch(x) && right.isMatch(x))

    def translateOr(left: Predicate[T], right: Predicate[T]): Predicate[T] =
      Predicate(x => left.isMatch(x) || right.isMatch(x))

    node match {
      case Is(FieldSpecOneNode(fs), ConstNode(v)) => translateIs(fs, v)
      case Is(ConstNode(v), FieldSpecOneNode(fs)) => translateIs(fs, v)
      case And(left, right) => translateAnd(translateNode(left), translateNode(right))
      case Or(left, right) => translateOr(translateNode(left), translateNode(right))
      case ConstNode(true) => Predicate(_ => true)
      case _ => error("anything else is not supported at the moment")
    }
  }

}
