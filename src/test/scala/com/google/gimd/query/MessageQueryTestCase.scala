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

import file.{File, FileType}
import org.junit.Test
import org.junit.Assert._

class MessageQueryTestCase {
  case class TreeNode(id: Int, name: String)
  object TreeNodeType extends UserType[TreeNode] {
    def toMessageBuffer(n: TreeNode) =
      new MessageBuffer().
        add("id", n.id).
        add("name", n.name)
    def toUserObject(m: Message): TreeNode =
      new TreeNode(
        m.one("id").intField.value,
        m.one("name").stringField.value
      )
    override def children = Seq(new NestedMember("node", TreeNodeType))
  }

  val root = new TreeNode(-1, "a")
  val child_0 = new TreeNode(0, "a")
  val child_1 = new TreeNode(1, "b")
  val child_2 = new TreeNode(2, "a")

  val child_msg0 = TreeNodeType.toMessage(child_0)
  val child_msg1 = TreeNodeType.toMessage(child_1)
  val child_msg2 = TreeNodeType.toMessage(child_2)

  val message = TreeNodeType.toMessageBuffer(root).
    add("node", child_msg0).
    add("node", child_msg1).
    add("node", child_msg2).readOnly

  @Test
  def queryParentWithChildren = {
    val queryResult = query((n: TreeNode) => n.name == "a")

    val expectedResult = Set(
      (PathHandle(Nil), root),
      (handle(("node", child_msg0)), child_0),
      (handle(("node", child_msg2)), child_2)
    )
    assertEquals(expectedResult, Set(queryResult.toList: _*))
  }

  @Test
  def queryChildren = {
    val queryResult = query((n: TreeNode) => n.name == "a" && n.id >= 0)

    val expectedResult = Set(
      (handle(("node", child_msg0)), child_0),
      (handle(("node", child_msg2)), child_2)
    )
    assertEquals(expectedResult, Set(queryResult.toList: _*))
  }

  @Test
  def queryParent = {
    val queryResult = query((n: TreeNode) => n.name == "a" && n.id < 0)

    val expectedResult = Set(
      (PathHandle(Nil), root)
    )
    assertEquals(expectedResult, Set(queryResult.toList: _*))
  }

  private def query(p: (TreeNode) => Boolean) = {
    import Predicate.functionLiteral2Predicate
    MessageQuery.simpleQuery(TreeNodeType, message, p)
  }

  private def handle(p: (String, Message)*) =
    PathHandle(p.map { case (name, msg) => (TreeNodeType, MessageField(name, msg)) } toList)
}
