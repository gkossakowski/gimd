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

import com.google.gimd.{FieldSpecOne, UserType}
import com.google.gimd.query.BooleanNodeOps.And
import reflect.Manifest

/**
 * Class that holds a Query AST defined for specific UserType.
 */
final class Query[W, U <: UserType[W]](val ut: U,
                                       val cond: List[Node[Boolean]])(implicit m: Manifest[W]) {

  def where(f: U => Node[Boolean]) = new Query[W,U](ut, f(ut) :: cond)

  def node: Node[Boolean] = cond.foldLeft[Node[Boolean]](ConstNode(true))(And)

  def predicate: Predicate[W] = PredicateBuilder.predicate(this)

}

object Query {

  //This implicit conversion does not work because types are not inferred, see:
  // http://thread.gmane.org/gmane.comp.lang.scala.user/25446
  implicit def userType2Query[W,U <: UserType[W]](ut: U)(implicit m: Manifest[W]) =
    new Query[W,U](ut, Nil)

  //a few conversions to Node[T]
  implicit def fieldSpecOne2Node[T:Manifest,F](fs: FieldSpecOne[T,F]) = FieldSpecOneNode(fs)
  implicit def nodeOps2Node[T](ops: NodeOps[T]) = ops.leftOperand
  implicit def const2Node[T](x: T) = ConstNode(x)
  implicit def arbitraryPredicate2Node[T](p: Predicate[T]) = PredicateNode(p)

  //a few lifts to traits that define operations one can apply to given Nodes.
  implicit def fieldSpecOne2BooleanNodeOps[T:Manifest](fs: FieldSpecOne[T,Boolean]) =
    new BooleanNodeOps {
      val leftOperand = FieldSpecOneNode(fs)
    }
  implicit def fieldSpecOne2AllNodeOps[T:Manifest,F](fs: FieldSpecOne[T,F]) = new AllNodeOps[F] {
    val leftOperand = FieldSpecOneNode(fs)
  }

  implicit def booleanConst2BooleanNodeOps(x: Boolean) = new BooleanNodeOps {
    val leftOperand = ConstNode(x)
  }
  implicit def const2AllNodeOps[T](x: T) = new AllNodeOps[T] {
    val leftOperand = ConstNode(x)
  }

  implicit def predicate2BooleanNodeOps(p: Predicate[_]) = new BooleanNodeOps {
    val leftOperand = PredicateNode(p)
  }

}
