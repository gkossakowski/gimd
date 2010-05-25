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
package com.google.gimd

import file.FileType

object TestTree {

  import com.google.gimd.UserType._
  sealed abstract class Node
  final case class Node1(name: String, children1: List[Node1], children2: List[Node2]) extends Node
  final case class Node2(id: Int, children1: List[Node1], children2: List[Node2]) extends Node

  object Node1Type extends UserType[Node1] {
    val name = FieldSpecOne("name", StringField, _.name)
    val fields = name :: Nil
    override def children = Seq(new NestedMember("child1", Node1Type),
      new NestedMember("child2", Node2Type))
    override def toMessageBuffer(obj: Node1) =
      super.toMessageBuffer(obj) ++
              obj.children1.map(x => Field("child1", Node1Type.toMessage(x))) ++
              obj.children2.map(x => Field("child2", Node2Type.toMessage(x)))
    def toUserObject(m: Message) = Node1(name(m), Nil, Nil)
  }

  object Node2Type extends UserType[Node2] {
    val id = FieldSpecOne("id", IntField, _.id)
    val fields = id :: Nil
    override def children = Seq(new NestedMember("child1", Node1Type),
      new NestedMember("child2", Node2Type))
    override def toMessageBuffer(obj: Node2) =
      super.toMessageBuffer(obj) ++
              obj.children1.map(x => Field("child1", Node1Type.toMessage(x))) ++
              obj.children2.map(x => Field("child2", Node2Type.toMessage(x)))
    def toUserObject(m: Message) = Node2(id(m), Nil, Nil)
  }

  object Node1FileType extends FileType[Node1] {
    val pathPrefix = Some("n1/")
    val pathSuffix = None
    val userType = Node1Type
    def name(m: Message) = userType.name(m)
  }

  object Node2FileType extends FileType[Node2] {
    val pathPrefix = Some("n2/")
    val pathSuffix = None
    val userType = Node2Type
    def name(m: Message) = FieldSpecOne2Int(userType.id)(m).toString
  }

}
