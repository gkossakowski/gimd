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

import com.google.gimd.ClassUtils

trait Predicate[T] {
  /**
   * Checks if predicate is applicable to given type represented by Class object.
   *
   * This way we can ensure that isMatch() always will get called with parameter of proper
   * type and logic using predicate can avoid expensive bean creation if types mismatch.
   */
  def isType[Q](inType: Class[Q]): Boolean =
    ClassUtils.beanTypeOf(getClass, classOf[Predicate[T]]).isAssignableFrom(inType)

  def isMatch(bean: T): Boolean
}

/**
 * This is companion object to Predicate trait that contains a few useful definitions
 * for predicate creation.
 *
 * It has a ScalaPredicate class which wraps function literal into class implementing
 * Predicate trait. It has also implicit conversion so it's enough to say:
 *
 *   import Predicate.functionLiteral2Predicate
 *
 * To use function literals anywhere Predicate is expected.
 */
object Predicate {

  import reflect.Manifest

  private abstract class ScalaPredicate[T](m: Manifest[T]) extends Predicate[T] {
    private val want = m

    override def isType[Q](inType: Class[Q]): Boolean =
      if (inType == null)
        false
      else if (Manifest.classType(inType) == want)
        true
      else if (inType.getInterfaces.exists(a => isType(a)))
        true
      else
        isType(inType.getSuperclass)
  }

  def apply[T](predicate: (T) => Boolean)(implicit m: Manifest[T]): Predicate[T] =
    new ScalaPredicate[T](m) {
      def isMatch(bean: T) = predicate(bean)
    }

  implicit def functionLiteral2Predicate[T](function: (T) => Boolean)
                                           (implicit m: Manifest[T]): Predicate[T] = apply(function)

}
