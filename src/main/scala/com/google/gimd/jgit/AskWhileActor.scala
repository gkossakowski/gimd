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

import actors.{TIMEOUT, Actor}
import math.max

/**
 * An actor that asks other actor for some answer until askWhile returns false. Then this answer is
 * sent back to responseTo actor. There is a timeOut for waiting for correct answer. If timeOut is
 * reached actor stops it's execution.
 */
class AskWhileActor(who: Actor, msg: Any, askWhile: Any => Boolean, responseTo: Actor,
                    timeOut: Long = 1000) extends Actor {

  start();

  def act() = {
    var satisfied = false
    var timedOut = false
    val startTime = System.currentTimeMillis
    who ! msg
    loopWhile(!satisfied && !timedOut) {
      reactWithin(max(startTime+timeOut-System.currentTimeMillis,0)) {
        case TIMEOUT => {
          timedOut = true
        }
        case answer => {
          if (!askWhile(answer)) {
            satisfied = true
            responseTo ! answer
          } else {
            who ! msg
          }
        }
      }
    }
  }

}
