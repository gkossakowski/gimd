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

/**
 * UserType[T] defines how to perform conversion between generic Message class and and type T.
 *
 * UserType[T] has children method used to define references to other UserTypes.
 */
abstract class UserType[T] {
  def userTypeClass: Class[T] = ClassUtils.beanTypeOf(this.getClass, classOf[UserType[T]])

  def toUserObject(itr: Message): T
  def toMessage(obj: T) = toMessageBuffer(obj).readOnly
  def toMessageBuffer(obj: T): MessageBuffer = new MessageBuffer ++ fields.map {
    case FieldSpec(name, f1, f2) => f1(name, f2(obj))
  }
  def fields: List[FieldSpec[T, _]]
  def children: Seq[NestedMember[_]] = Seq()
}

case class FieldSpec[T, F](name: String, f1: (String, F) => Field, f2: T => F)
