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

object MessageQuery {

  def simpleQuery[U, W](ut: UserType[W], m: Message, p: Predicate[U], parent: Handler,
                        factory: MessageHandler.Factory): List[(MessageHandler,U)] = {
    val handler = factory(parent, m)

    val matched = if (p.isType(ut.userTypeClass)) {
      val value = ut.toUserObject(m).asInstanceOf[U]

      if (p.isMatch(value))
        List((handler, value))
      else
        Nil
    } else Nil

    val childMatches = (for {
      nm <- ut.children
      childMessageFields = Message.filterMessageFields(m.all(nm.name))
      matches = childMessageFields.flatMap(simpleQuery(nm.userType, _, p, handler, factory))
    } yield matches).flatMap(identity[List[(MessageHandler,U)]])

    matched ++ childMatches
  }

}
