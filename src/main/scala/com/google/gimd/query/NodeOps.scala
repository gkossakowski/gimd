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

/**
 * Base trait for all traits defining operations on given Node[T] stored in leftOperand.
 */
trait NodeOps[T] {
  val leftOperand: Node[T]
}

/**
 * Operations applicable to all Node[T] instances.
 *
 * Since all primitive data types used in Gimd have equality and (natural) total order defined
 * we can provide operators related to equality and ordering here.
 */
trait AllNodeOps[T] extends NodeOps[T] {
  import AllNodeOps._
  def is(right: Node[T]) = Is(leftOperand, right)
  def ===(right: Node[T]) = is(right)
  def isNot(right: Node[T]) = !is(right)
  def !==(right: Node[T]) = isNot(right)

  def <(right: Node[T]) = Relational("<", leftOperand, right)
  def >(right: Node[T]) = !(<(right)) && !==(right)
  def <=(right: Node[T]) = <(right) || ===(right)
  def >=(right: Node[T]) = !(<(right))
}

object AllNodeOps {
  case class Is[T](left: Node[T], right: Node[T]) extends Node[Boolean] with BooleanNodeOps {
    val leftOperand = this
  }

  case class Relational[T](name: String, left: Node[T], right: Node[T]) extends Node[Boolean] with BooleanNodeOps {
    val leftOperand = this
  } 
}

/**
 *  This trait defines boolean operations thus is applicable to Node[Boolean]
 */
trait BooleanNodeOps extends AllNodeOps[Boolean] {
  import BooleanNodeOps._
  def && (right: Node[Boolean]) = And(leftOperand, right)
  def || (right: Node[Boolean]) = Or(leftOperand, right)
  def unary_! = Not(leftOperand)
}

object BooleanNodeOps {
  case class And(left: Node[Boolean], right: Node[Boolean]) extends Node[Boolean] with BooleanNodeOps {
    val leftOperand = this
  }

  case class Or(left: Node[Boolean], right: Node[Boolean]) extends Node[Boolean] with BooleanNodeOps {
    val leftOperand = this
  }

  case class Not(left: Node[Boolean]) extends Node[Boolean] with BooleanNodeOps {
    val leftOperand = this
  }
}
