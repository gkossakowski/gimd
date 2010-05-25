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

/**
 * Simple trait that performance tests can mix-in to have access to some utility functions. 
 */
trait PerfTest {

  protected def measureTime(n: Int, action: () => Unit): (Double, Double) = {
    import java.util.Date
    val times = (1 to n) map { _ =>
      val time1 = (new Date).getTime
      action()
      val time2 = (new Date).getTime
      time2-time1
    }
    val avg = times.sum / n
    val variance = times.map(x => (avg - x)*(avg - x)).sum / n
    (avg, variance)
  }

  protected def formatInfo(what: String, avgVar: (Double, Double)): String = {
    val (avg, variance) = avgVar
    "The action '%s' exectued in %f ms on average with %f ms of variance".
            format(what, avg, variance)
  }
  
}
