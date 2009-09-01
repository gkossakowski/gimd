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

import collection.immutable.TreeSet
import file.File
import DatabaseModification._
import query.{Handle, CompleteHandle, PathHandle}

/**
 * <p>Immutable datastructure that represents set of Modifications applied to Gimd database.</p>
 *
 * <p>Modification is either: insert, modify or remove.</p>
 *
 * <p>It's possible that one will supply two or more conflicting modifications.
 *    Rules are following:</p>
 *    <ul>
 *     <li>Remove(handle) is in conflict with either Insert(handle, ...) or Modify(handle, ...) or
 *         another Remove(handle)</li>
 *     <li>Modify(handle, ...) is in conflict with Modify(handle)</li>
 *     <li>Insert(handle, ...) is not in conflict with another Insert(handle, ...) with exception if
 *         you try to insert exactly the same userObject twice</li>
 *    </ul>
 *
 * <p>Since order of supplied modifications is not preserved when method <code>reduce</code> is
 *    being called then List(m_1, m_2, ..., m_n) of modifications has conflict if any permutation of
 *    of that list has a conflicting modification according to rules outlined above.</p>
 *
 * <p>An empty modification can be obtained using <code>DatabaseModification.empty</code></p>
 */
final class DatabaseModification private(commands: Map[File[_], Modification]) {

  /**
   * PhantomMessage is used as placeholder when insert command is translated to modification of
   * non-existing (phantom) message.
   */
  private final case class PhantomMessage(replaceWith: Message) extends Message(TreeSet.empty)
  private object PhantomMessageType extends UserType[Unit] {
    def toUserObject(itr: Message) = ()
    def fields = Nil
  }

  /**
   * <p>Inserts userObject as a child of other object that handle is pointing at in position
   *    specified by nestedMember.<p>
   *
   * <p>Since one doesn't have an access to Handle to freshly inserted userObject there is no
   *    possibility to insert another user object under supplied userObject. This limitation should
   *    be addressed in the future.</p>
   *
   * @returns DatabaseModification with insertion scheduled for application
   * @throws ConflictingModificationException see rules outlined above
   * @throws IllegalArgumentException if supplied nestedMember is not a NestedMember of UserType
   *         of object that handle is pointing at
   */
  def insert[T,U](handle: Handle[U], nestedMember: NestedMember[T], userObject: T):
    DatabaseModification = insert(completeHandle(handle), nestedMember, userObject)

  private def insert[T,U](handle: CompleteHandle[U], nestedMember: NestedMember[T], userObject: T):
    DatabaseModification = {
    val userType = userTypeOf(handle)
    if (!userType.children.contains(nestedMember))
      throw new IllegalArgumentException(
        "Passed nestedMember + '%1s' is not child of '%2s'.".format(nestedMember, userType))
    val file = handle.file
    val message = nestedMember.userType.toMessage(userObject)
    val modification = modificationOf(file)
    val cmd = ModifyCommand(message)
    val phantomField = MessageField(nestedMember.name, PhantomMessage(message))
    val phantomHandle = PathHandle(handle.pathHandle.path :::
            List((PhantomMessageType, phantomField)))
    new DatabaseModification(commands(file) = modification.addCommand(cmd, phantomHandle))
  }

  /**
   * <p>Modifies object that handle is pointing at by using supplied userObject.</p>
   *
   * <p>This operation is not recursive. This limitatiom might be fixed in a future.</p>
   *
   * @returns DatabaseModification with modification scheduled for application
   * @throws ConflictingModification see rules outlined above
   */
  def modify[T](handle: Handle[T], userObject: T): DatabaseModification =
    modify(completeHandle(handle), userObject)

  private def modify[T](handle: CompleteHandle[T], userObject: T): DatabaseModification = {
    val file = handle.file
    val userType = userTypeOf(handle, userObject)
    val message = userType.toMessage(userObject)
    val modification = modificationOf(file)
    val cmd = ModifyCommand(message)
    new DatabaseModification(commands(file) = modification.addCommand(cmd, handle.pathHandle))
  }

  /**
   * Removes user object that handle is pointing at.
   *
   * @returns DatabaseModification with removal scheduled for application
   * @throws ConflictingModificationException see rules outlined above
   */
  def remove[T](handle: Handle[T]): DatabaseModification = remove(completeHandle(handle))

  private def remove[T](handle: CompleteHandle[T]): DatabaseModification = {
    val file = handle.file
    val modification = modificationOf(file)
    val cmd = RemoveCommand
    new DatabaseModification(commands(file) = modification.addCommand(cmd, handle.pathHandle))
  }

  /**
   * Applies all modifications and returns the result.
   *
   * @returns Map[File[_], Option[Message]]. If file points at None it means that top-level message
   *          has been removed and File itself should be removed from Database.
   */
  def reduce: collection.Map[File[_], Option[Message]] = commands.mapElements(_.reduce)

  private def userTypeOf[T](handle: CompleteHandle[T], userObject: T): UserType[T] = {
    val userType = userTypeOf(handle)
    val userObjectUpCasted = userObject.asInstanceOf[AnyRef]
    if (userType.userTypeClass == userObjectUpCasted.getClass)
      userType.asInstanceOf[UserType[T]]
    else {
      val errorMsg = "userObject is of invalid type. Expected: %1s but got: %2s".
              format(userType.userTypeClass, userObjectUpCasted.getClass)
      throw new IllegalArgumentException(errorMsg)
    }
  }

  private def userTypeOf(handle: CompleteHandle[_]): UserType[_] =
    handle.pathHandle.path.lastOption match {
      case Some((ut, _)) => ut
      case None => handle.file.fileType.userType
    }

  private def modificationOf(file: File[_]): Modification =
    commands.getOrElse(file, ImplicitNode(file.fileType.userType, file.message, Map.empty))

  private def completeHandle[T](handle: Handle[T]): CompleteHandle[T] = handle match {
    case x: CompleteHandle[_] => x
  }
}

object DatabaseModification {

  /**
   * Empty modification. This way is always a starting point for any modifications as constructor of
   * DatabaseModification class is private.
   */
  val empty = new DatabaseModification(Map.empty)

  private def conflictingModificationException(cmd: ModificationCommand,
                                               otherModif: Modification) =
    new ConflictingModificationException(
      "Command '%1s'is in conflict with existing modification '%2s'".format(cmd, otherModif)
    )

  private sealed abstract class ModificationCommand
  private object RemoveCommand extends ModificationCommand
  private final case class ModifyCommand(newMessage: Message) extends ModificationCommand

  private[modification] sealed abstract class Modification(val oldMessage: Message) {

    def addCommand(cmd: ModificationCommand, handle: PathHandle[_]): Modification

    def reduce: Option[Message]
  }

  private abstract class ModificationTree
        (val userType: UserType[_],
         override val oldMessage: Message,
         val children: Map[MessageField, Modification]) extends Modification(oldMessage) {

    def transformAccordingToCommand(cmd: ModificationCommand): Modification

    def addChild(key: MessageField, child: Modification): Modification

    def addCommand(cmd: ModificationCommand, handle: PathHandle[_]): Modification =
      handle match {
      case PathHandle(Nil) => transformAccordingToCommand(cmd)
      case PathHandle((ut, mf) :: xs) => {
        val child = nodeOf(mf)
        addChild(mf, child.addCommand(cmd, PathHandle(xs)))
      }
    }

    protected def nodeOf(mf: MessageField) =
      children.getOrElse(mf, ImplicitNode(userType, mf.value, Map.empty))

    /**
     * @returns (edits: Map[MessageField, Message], removals: Set[MessageField])
     */
    protected def reduceChildren: (Map[MessageField, Message], Set[MessageField]) = {
      val reducedChildren = children.mapElements(_.reduce)
      val initMap: Map[MessageField, Message] = Map.empty
      val initSet: Set[MessageField] = Set.empty
      reducedChildren.foldLeft((initMap, initSet)) {
        case ((map, set), (key, None)) => (map, set + key)
        case ((map, set), (key, Some(x))) => (map(key) = x, set)
      }
    }
  }

  private final case class ImplicitNode
        (override val userType: UserType[_],
         override val oldMessage: Message,
         override val children: Map[MessageField, Modification])
          extends ModificationTree(userType, oldMessage, children) {

    def addChild(key: MessageField, child: Modification) =
      ImplicitNode(userType, oldMessage, children(key) = child)

    def transformAccordingToCommand(cmd: ModificationCommand) = cmd match {
      case RemoveCommand => if (children == Map.empty)
                              Remove(oldMessage)
                            else
                              throw conflictingModificationException(cmd, this)
      case ModifyCommand(newMessage) => EditNode(userType, oldMessage, newMessage, children)
    }

    def reduce: Option[Message] = {
      val (edits, removals) = reduceChildren
      //it's strange that Sorted.filter is not overridden to return Sorted instead of
      //Iterable. Thus we cannot take advantage of the fact that toBeCopied is already
      //sorted collection so sorting can be avoided
      val toBeCopied = oldMessage.filter {
        case x: MessageField => !(edits.contains(x) || removals.contains(x))
        case _ => true
      }
      val editedRecursiveFields = edits.map { case (mf, msg) => MessageField(mf.name, msg) }
      val resultMsg = ((new MessageBuffer) ++ toBeCopied ++ editedRecursiveFields).readOnly
      Some(resultMsg)
    }
  }

  private final case class EditNode
        (override val userType: UserType[_],
         override val oldMessage: Message,
         val newMessage: Message,
         override val children: Map[MessageField, Modification])
          extends ModificationTree(userType, oldMessage, children) {

    def addChild(key: MessageField, child: Modification) =
      EditNode(userType, oldMessage, newMessage, children(key) = child)

    def transformAccordingToCommand(cmd: ModificationCommand) =
      throw conflictingModificationException(cmd, this)

    def reduce: Option[Message] = {
      val (edits, removals) = reduceChildren
      val definedFields = Set(userType.fields.map(_.name): _*)
      val toBeCopied = oldMessage.filter {
        case x: MessageField => !(edits.contains(x) || removals.contains(x))
        case x: Field => !definedFields.contains(x.name)
      }
      val editedRecursiveFields = edits.map { case (mf, msg) => MessageField(mf.name, msg) }
      val msgBuffer = new MessageBuffer
      val resultMsg = (msgBuffer ++ toBeCopied ++ editedRecursiveFields ++ newMessage).readOnly
      Some(resultMsg)
    }
  }

  private final case class Remove(override val oldMessage: Message)
          extends Modification(oldMessage) {

    def addCommand(cmd: ModificationCommand, handle: PathHandle[_]): Modification =
      throw conflictingModificationException(cmd, this)

    def reduce = None
  }
}
