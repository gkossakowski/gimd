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

package com.google.gimd.modification


import com.google.gimd._
import com.google.gimd.UserType._
import com.google.gimd.file.{FileType, File}
import com.google.gimd.query._
import org.junit.Test
import org.junit.Assert._

final class ModificationTestCase {

  val nestedMember = new NestedMember("node", TreeNodeType)

  case class TreeNode(id: Int, name: String)
  object TreeNodeType extends UserType[TreeNode] {
    val id = FieldSpecOne("id", IntField, _.id)
    val name = FieldSpecOne("name", StringField, _.name)
    def fields = id :: name :: Nil
    override def children = Seq(nestedMember)
    def toUserObject(m: Message) = new TreeNode(id(m), name(m))
  }
  object MockFileType extends FileType[TreeNode] {
    val pathPrefix = None
    val pathSuffix = None
    val userType = TreeNodeType
    def name(m: Message) = userType.id(m).toString
  }
  case class MockFile(message: Message) extends File[TreeNode] {
    val path = ""
    val fileType = MockFileType
    val userObject = fileType.userType.toUserObject(message)
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
  def insertUnderEmptyPath {
    val file = MockFile(message)
    val handle = CompleteHandle(file, PathHandle.empty)
    val child_3 = TreeNode(3, "a")
    val child_msg3 = TreeNodeType.toMessage(child_3)
    val modification = DatabaseModification.empty.insert(handle, nestedMember, child_3)
    val (modFiles, _) = modification.reduce
    val newTopMessage = modFiles(file).get
    val expectedTopMessage = TreeNodeType.toMessageBuffer(root).
                              add("node", child_msg0).
                              add("node", child_msg1).
                              add("node", child_msg2).
                              add("node", child_msg3).readOnly
    assertEquals(expectedTopMessage, newTopMessage)
  }

  @Test
  def insertNewFile {
    val modification = DatabaseModification.empty.insertFile(MockFileType, root)
    val (_, newFiles) = modification.reduce
    val root_msg = TreeNodeType.toMessage(root)
    assertEquals(List((MockFileType, root_msg)), newFiles)
  }

  @Test
  def insertUnderNonEmptyPath {
    val file = MockFile(message)
    val handle = CompleteHandle(file, path(child_msg1))
    val child_3 = TreeNode(3, "a")
    val child_msg3 = TreeNodeType.toMessage(child_3)
    val modification = DatabaseModification.empty.insert(handle, nestedMember, child_3)
    val (modFiles, _) = modification.reduce
    val newTopMessage = modFiles(file).get
    val expectedTopMessage = TreeNodeType.toMessageBuffer(root).
                              add("node", child_msg0).
                              add("node", child_msg1 + MessageField("node", child_msg3)).
                              add("node", child_msg2).readOnly
    assertEquals(expectedTopMessage, newTopMessage)
  }

  @Test
  def removeLeaf {
    val file = MockFile(message)
    val handle = CompleteHandle(file, path(child_msg1))
    val modification = DatabaseModification.empty.remove(handle)
    val (modFiles, _) = modification.reduce
    val newTopMessage = modFiles(file).get
    val expectedTopMessage = TreeNodeType.toMessageBuffer(root).
                              add("node", child_msg0).
                              add("node", child_msg2).readOnly
    assertEquals(expectedTopMessage, newTopMessage)
  }

  @Test
  def removeTop {
    val file = MockFile(message)
    val handle = CompleteHandle(file, PathHandle.empty)
    val modification = DatabaseModification.empty.remove(handle)
    assertEquals(None, modification.reduce._1(file))
  }

  @Test
  def editTop {
    val file = MockFile(message)
    val handle = CompleteHandle[TreeNode](file, PathHandle.empty)
    val newRoot = TreeNode(-1, "c")
    val modification = DatabaseModification.empty.modify(handle, newRoot)
    val (modFiles, _) = modification.reduce
    val newTopMessage = modFiles(file).get
    val expectedTopMessage = TreeNodeType.toMessageBuffer(newRoot).
                              add("node", child_msg0).
                              add("node", child_msg1).
                              add("node", child_msg2).readOnly
    assertEquals(expectedTopMessage, newTopMessage)
  }

  @Test
  def editChildAndInsertUnderneath {
    val file = MockFile(message)
    val handle = CompleteHandle[TreeNode](file, path(child_msg1))
    val newChild_1 = TreeNode(1, "c")
    val newChild_msg1 = TreeNodeType.toMessage(newChild_1)
    val child_3 = TreeNode(3, "a")
    val child_msg3 = TreeNodeType.toMessage(child_3)
    val modification = DatabaseModification.empty.
                        modify(handle, newChild_1).
                        insert(handle, nestedMember, child_3)
    val (modFiles, _) = modification.reduce
    val newTopMessage = modFiles(file).get
    val expectedTopMessage = TreeNodeType.toMessageBuffer(root).
                              add("node", child_msg0).
                              add("node", newChild_msg1 + MessageField("node", child_msg3)).
                              add("node", child_msg2).readOnly
    assertEquals(expectedTopMessage, newTopMessage)
  }

  @Test
  def insertUnderneathChildAndEdit {
    val file = MockFile(message)
    val handle = CompleteHandle[TreeNode](file, path(child_msg1))
    val newChild_1 = TreeNode(1, "c")
    val newChild_msg1 = TreeNodeType.toMessage(newChild_1)
    val child_3 = TreeNode(3, "a")
    val child_msg3 = TreeNodeType.toMessage(child_3)
    val modification = DatabaseModification.empty.
                        insert(handle, nestedMember, child_3).
                        modify(handle, newChild_1)
    val (modFiles, _) = modification.reduce
    val newTopMessage = modFiles(file).get
    val expectedTopMessage = TreeNodeType.toMessageBuffer(root).
                              add("node", child_msg0).
                              add("node", newChild_msg1 + MessageField("node", child_msg3)).
                              add("node", child_msg2).readOnly
    assertEquals(expectedTopMessage, newTopMessage)
  }

  @Test(expected = classOf[ConflictingModificationException])
  def removeTopAndInsertUnderneath {
    val file = MockFile(message)
    val handle = CompleteHandle(file, PathHandle.empty)
    val child_3 = TreeNode(3, "a")
    DatabaseModification.empty.remove(handle).insert(handle, nestedMember, child_3)
  }

  @Test(expected = classOf[ConflictingModificationException])
  def editTopTwoTimes {
    val file = MockFile(message)
    val handle = CompleteHandle[TreeNode](file, PathHandle.empty)
    DatabaseModification.empty.modify(handle, root).modify(handle, root)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def editWrongType {
    val file = MockFile(message)
    val handle = CompleteHandle[Unit](file, PathHandle.empty)
    DatabaseModification.empty.modify(handle, ())
  }

  @Test(expected = classOf[IllegalArgumentException])
  def passWrongNestedMember {
    val file = MockFile(message)
    val handle = CompleteHandle[TreeNode](file, PathHandle.empty)
    val child_3 = TreeNode(3, "a")
    DatabaseModification.empty.insert(handle, new NestedMember("wrong", TreeNodeType), child_3)
  }

  private def path(msgs: Message*) =
    PathHandle(List.fill(msgs.size)(TreeNodeType) zip msgs.map(MessageField("node", _)).toList)

}
