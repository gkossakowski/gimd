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

import com.google.gimd._
import com.google.gimd.UserType._
import com.google.gimd.query.AllNodeOps._
import com.google.gimd.query.BooleanNodeOps._
import com.google.gimd.query.Query._
import org.junit.Test
import org.junit.Assert._

class QueryASTTestCase {

  case class TreeNode(id: Int, name: String)
  object TreeNodes extends UserType[TreeNode] {
    val id = FieldSpecOne("id", IntField, _.id)
    val name = FieldSpecOne("name", StringField, _.name)
    def fields = id :: name
    override def children = Seq(new NestedMember("node", TreeNodes))
    def toUserObject(m: Message) = new TreeNode(id(m), name(m))
  }

  @Test
  def equalityOperators = {
    val q1 = TreeNodes.query where { _.name is "Joe" }
    val q2 = TreeNodes.query where { _.name === "Joe" }
    val expected = Is(FieldSpecOneNode(TreeNodes.name), ConstNode("Joe")) :: Nil
    assertEquals(expected, q1.cond)
    assertEquals(expected, q2.cond)
  }

  @Test
  def nonEqualityOperators {
    val q1 = TreeNodes.query where { _.name !== "Joe" }
    val q2 = TreeNodes.query where { _.name isNot "Joe" }
    val expected = Not(Is(FieldSpecOneNode(TreeNodes.name), ConstNode("Joe"))) :: Nil
    assertEquals(expected, q1.cond)
    assertEquals(expected, q2.cond)
  }

  @Test
  def booleanOperators {
    val left = TreeNodes.name is "Joe"
    val right = ConstNode(false)
    val qAnd = TreeNodes.query where { x => (x.name is "Joe") && false }
    val qOr = TreeNodes.query where { x => (x.name is "Joe") || false }
    val qNot = TreeNodes.query where { x => !(x.name is "Joe") }
    assertEquals(And(left, right) :: Nil, qAnd.cond)
    assertEquals(Or(left, right) :: Nil, qOr.cond)
    assertEquals(Not(left) :: Nil, qNot.cond)
  }

  @Test
  def orderingOperators {
    val q1 = TreeNodes.query where { _.name < "Joe" }
    val q2 = TreeNodes.query where { _.name <= "Joe" }
    val q3 = TreeNodes.query where { _.name > "Joe" }
    val q4 = TreeNodes.query where { _.name >= "Joe" }
    val lt = Relational("<", FieldSpecOneNode(TreeNodes.name), ConstNode("Joe"))
    val is = Is(FieldSpecOneNode(TreeNodes.name), ConstNode("Joe"))
    assertEquals(lt :: Nil, q1.cond)
    assertEquals(Or(lt, is) :: Nil, q2.cond)
    assertEquals(And(Not(lt), Not(is)) :: Nil, q3.cond)
    assertEquals(Not(lt) :: Nil, q4.cond)
  }

  @Test
  def predicateNode {
    val p = Predicate[TreeNode](_.name == "Joe")
    val q = TreeNodes.query where { x => p && true }
    assertEquals(And(PredicateNode(p), ConstNode(true)) :: Nil, q.cond)
  }

}
