// Copyright (C) 2009 The Android Open Source Project
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

/**
 * Class that holds a Query AST defined for specific UserType.
 */
final class Query[U <: UserType[_]](val ut: U, val cond: List[Node[Boolean]]) {

  def where(f: U => Node[Boolean]) = new Query[U](ut, f(ut) :: cond)

}

object Query {

  //this alias is needed due to bug in Scala 2.7.x. It's been fixed in 2.8.0 so once we switch to it
  //this can be removed
  type UserType_ = UserType[_]

  implicit def userType2Query[U <: UserType_](ut: U) = new Query[U](ut, Nil)

  //a few conversions to Node[T]
  implicit def fieldSpecOne2Node[T](fs: FieldSpecOne[_,T]) = FieldSpecOneNode(fs)
  implicit def nodeOps2Node[T](ops: NodeOps[T]) = ops.leftOperand
  implicit def const2Node[T](x: T) = ConstNode(x)
  implicit def arbitraryPredicate2Node[T](p: Predicate[T]) = PredicateNode(p)

  //a few lifts to traits that define operations one can apply to given Nodes.
  implicit def fieldSpecOne2BooleanNodeOps(fs: FieldSpecOne[_,Boolean]) = new BooleanNodeOps {
    val leftOperand = FieldSpecOneNode(fs)
  }
  implicit def fieldSpecOne2AllNodeOps[T](fs: FieldSpecOne[_,T]) = new AllNodeOps[T] {
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
