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

package com.google.gimd


import java.lang.reflect.{Type, ParameterizedType}

object ClassUtils {

  def beanTypeOf[B,T,R](base: Class[B], target: Class[T]): Class[R] =
    beanTypeOf(base, base, target)

  private def beanTypeOf[B,C,T,R](base: Class[B], current: Class[C], target: Class[T]): Class[R] =
    if (current.getSuperclass == target)
      beanTypeOf(base, current.getGenericSuperclass)
    else if (current.getSuperclass != null)
      beanTypeOf(base, current.getSuperclass, target)
    else
      throw cannotGuessType(base)

  private def beanTypeOf[Q,R](base: Class[Q], t: Type): Class[R] =
    t match {
      case t: ParameterizedType =>
        t.getActualTypeArguments()(0) match {
          case c: Class[R] => c
          case wrongType   => throw cannotGuessType(base)
        }
      case wrongType => throw cannotGuessType(base)
    }

  private def cannotGuessType[Q](base: Class[Q]): IllegalStateException =
    new IllegalStateException("Cannot guess bean type wanted by " + base.getName)

}
