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

import query.Query
import reflect.Manifest

/**
 * UserType[T] defines how to perform conversion between generic Message class and and type T.
 *
 * UserType[T] has children method used to define references to other UserTypes.
 */
abstract class UserType[T] {
  def userTypeClass: Class[T] = ClassUtils.beanTypeOf(this.getClass, classOf[UserType[T]])

  def toUserObject(itr: Message): T
  def toMessage(obj: T) = toMessageBuffer(obj).readOnly
  def toMessageBuffer(obj: T): MessageBuffer =
    new MessageBuffer ++ fields.flatMap(fieldSpec2Field(obj, _).toList)
  def fields: List[FieldSpec[T, _]]
  def children: Seq[NestedMember[_]] = Seq()

  def query(implicit m: Manifest[T]): Query[T, this.type] = new Query[T, this.type](this, Nil)

  protected final def FieldSpecOne[F](name: String, f1: (String, F) => Field, f2: T => F) =
    new FieldSpecOne(name, f1, f2)

  protected final def FieldSpecOption[F](name: String, f1: (String, F) => Field, f2: T => F,
                                      default: F) = new FieldSpecOption(name, f1, f2, default)

  private def fieldSpec2Field[F](obj: T, x: FieldSpec[T, F]) =
    x.fieldFactoryOption(x.name, x.f2(obj))
}

object UserType {

  implicit def FieldSpecOne2Int[T](spec: FieldSpecOne[T, Int]) =
    (m: Message) => m.one(spec.name).intField.value

  implicit def FieldSpecOne2String[T](spec: FieldSpecOne[T, String]) =
    (m: Message) => m.one(spec.name).stringField.value

  implicit def FieldSpecOne2Timestamp[T](spec: FieldSpecOne[T, Timestamp]) =
    (m: Message) => m.one(spec.name).timestampField.value

  implicit def FieldSpecOne2Long[T](spec: FieldSpecOne[T, Long]) =
    (m: Message) => m.one(spec.name).longField.value

  implicit def FieldSpecOne2BigInt[T](spec: FieldSpecOne[T, BigInt]) =
    (m: Message) => m.one(spec.name).bigIntField.value

  implicit def FieldSpecOne2BigDecimalField[T](spec: FieldSpecOne[T, BigDecimal]) =
    (m: Message) => m.one(spec.name).bigDecimalField.value

  implicit def FieldSpecOption2Int[T](spec: FieldSpecOption[T, Int]) =
    (m: Message) => m.oneOption(spec.name).map(_.intField.value).getOrElse(spec.defaultValue)

  implicit def FieldSpecOption2String[T](spec: FieldSpecOption[T, String]) =
    (m: Message) => m.oneOption(spec.name).map(_.stringField.value).getOrElse(spec.defaultValue)

  implicit def FieldSpecOption2Timestamp[T](spec: FieldSpecOption[T, Timestamp]) =
    (m: Message) => m.oneOption(spec.name).map(_.timestampField.value).getOrElse(spec.defaultValue)

  implicit def FieldSpecOption2Long[T](spec: FieldSpecOption[T, Long]) =
    (m: Message) => m.oneOption(spec.name).map(_.longField.value).getOrElse(spec.defaultValue)

  implicit def FieldSpecOption2BigInt[T](spec: FieldSpecOption[T, BigInt]) =
    (m: Message) => m.oneOption(spec.name).map(_.bigIntField.value).getOrElse(spec.defaultValue)

  implicit def FieldSpecOption2BigDecimalField[T](spec: FieldSpecOption[T, BigDecimal]) =
    (m: Message) => m.oneOption(spec.name).map(_.bigDecimalField.value).getOrElse(spec.defaultValue)
}

abstract sealed class FieldSpec[T, F](val name: String,
                                      val fieldFactoryOption: (String, F) => Option[Field],
                                      val f2: T => F)

final case class FieldSpecOne[T, F](override val name: String,
                                    val fieldFactory: (String, F) => Field,
                                    override val f2: T => F)
  extends FieldSpec(name,
    (name: String, value: F) => Some(fieldFactory(name, value)),
    { userObject: T =>
      val result = f2(userObject)
      if (result == null)
        error("UserObject '1%s' returned null for field '%2s'.".format(userObject, name))
      else
       result
    })

final case class FieldSpecOption[T, F](override val name: String,
                                       val fieldFactory: (String, F) => Field,
                                       override val f2: T => F,
                                       val defaultValue: F)
  extends FieldSpec(name,
    (name: String, value: F) => if (value != defaultValue)
                                  Some(fieldFactory(name, value))
                                else
                                  None,
    f2)


//TODO FieldSpecMany has to be implemented in a future


