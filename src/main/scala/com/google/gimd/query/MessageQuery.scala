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

import com.google.gimd.{MessageField, Message, UserType}

object MessageQuery {
  def simpleQuery[U, W](ut: UserType[W], m: Message, p: Predicate[U]):
   Iterator[(PathHandle[U],U)] = {
    val children = queryChildren(ut, m, p)
    val self = querySelf(ut, m, p)
    if (self.hasNext)
      self ++ children
    else
      children
  }

  private def querySelf[U, W](ut: UserType[W], m: Message, p: Predicate[U]) =
    if (p.isType(ut.userTypeClass)) {
      val obj = ut.toUserObject(m).asInstanceOf[U]
      if (p.isMatch(obj))
        Iterator.single( (PathHandle(Nil), obj) )
      else
        Iterator.empty
    } else
      Iterator.empty

  private def queryChildren[U, W](ut: UserType[W], m: Message, p: Predicate[U]) =
    // TODO if ut.children was sorted we could merge against the sorted
    // property of message and produce a faster join between the two.
    //
    for {
      member <- ut.children.iterator
      field <- m.allOfVariant[MessageField](member.name).iterator
      (PathHandle(xs), userObject) <- simpleQuery(member.userType, field.value, p)
    } yield (PathHandle[U]((ut, field) :: xs), userObject)
}
