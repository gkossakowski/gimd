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

import com.google.gimd.FieldSpecOne

/**
 * Base class for all nodes in Query AST.
 *
 * NOTE: As for now Node does only track the type (T) of the value that comes from
 * evaluating expression held by Node given specific instance defined by UserType.
 * However, Node does not track the type of UserType so it's possible that Node's
 * expression would be applied to wrong instance. This will be fixed in a future.
 */
abstract class Node[T]

/**
 * Node that stores FieldSpec. This node acts as placeholder in query for a given field of specific instance
 * defined by UserType.
 */
final case class FieldSpecOneNode[T](val fieldSpecOne: FieldSpecOne[_, T]) extends Node[T]

/**
 * Node that holds a constant value of type T.
 */
final case class ConstNode[T](val value: T) extends Node[T]

/**
 * Node that holds Predicate which (in turn) holds arbitrary function T => Boolean where T is a type
 * of instance that UserType is bound to.
 */
final case class PredicateNode[T](p: Predicate[T]) extends Node[Boolean]
