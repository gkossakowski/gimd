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
package com.google.gimd.text

import org.scalacheck.Shrink
import Shrink._
import com.google.gimd._

/**
 * Shrinks fields and messages by random cuts in values and structure of Messages.
 */
object MessageShrinker {

  implicit def shrinkMessage: Shrink[Message] = {
    def shrinkFields(xs: List[Field]) = shrink(xs)
    Shrink {
      case m: Message => {
        val shrunkMsgs = for (shrunkFields <- shrinkFields(m.toList)) yield Message(shrunkFields)
        //flattening messages means copying fields from nested message to parent message
        val flattenedMsgs = {
          val (xs, ys) = m.toList.partition(_.isInstanceOf[MessageField])
          for {
            msgsFields <- shrinkFields(xs)
            val nestedCollapsedShrunkFields = msgsFields.flatMap{
              case MessageField(_, nestedShrunkFields) => nestedShrunkFields
            }
            shrunkYs <- Stream.cons(List(), shrinkFields(ys))
          } yield Message(shrunkYs ++ nestedCollapsedShrunkFields)
        }
        shrunkMsgs append flattenedMsgs
      }
    }
  }

  implicit def shrinkField: Shrink[Field] = Shrink {
    case BigDecimalField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield BigDecimalField(sn, v)) append
      (for (sv <- shrink(v)) yield BigDecimalField(n, sv))
    }
    case BigIntField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield BigIntField(sn, v)) append
      (for (sv <- shrink(v)) yield BigIntField(n, sv))
    }
    case IntField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield IntField(sn, v)) append
      (for (sv <- shrink(v)) yield IntField(n, sv))
    }
    case LongField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield LongField(sn, v)) append
      (for (sv <- shrink(v)) yield LongField(n, sv))
    }
    case MessageField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield MessageField(sn, v)) append
      (for (sv <- shrink(v)) yield MessageField(n, sv))
    }
    case StringField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield StringField(sn, v)) append
      (for (sv <- shrink(v)) yield StringField(n, sv))
    }
    case TimestampField(n, v) => {
      (for (sn <- shrink(n) if !sn.isEmpty) yield TimestampField(sn, v)) append
      (for (sv <- shrink(v)) yield TimestampField(n, sv))
    }
  }
  
}
