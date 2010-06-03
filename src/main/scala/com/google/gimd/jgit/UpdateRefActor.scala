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
package com.google.gimd.jgit

import org.eclipse.jgit.lib.{ObjectId, Repository}
import UpdateRefActor._
import actors.{TIMEOUT, Actor}
import Actor._
import org.eclipse.jgit.lib.RefUpdate.Result

class UpdateRefActor(val branch: JGitBranch) {

  lazy val worker = actor {
    loop {
      react {
        case UpdateRef(newCommit) => {
          val refUpdate = branch.repository.updateRef(branch.name)
          refUpdate.setNewObjectId(newCommit)
          reply(refUpdate.update())
        }
      }
    }
  }

  def awaitAnswer(timeOut: Int): Actor = actor {
    react {
      case q => {
        val askWhileActor = new AskWhileActor(worker, q, (_ == Result.LOCK_FAILURE),
          Actor.self, timeOut)
        askWhileActor.start()
        reply {
          receiveWithin(timeOut) {
            case x: Result => x
          }
        }
      }
    }
  }

}

object UpdateRefActor {

  case class UpdateRef(newCommit: ObjectId)

}
