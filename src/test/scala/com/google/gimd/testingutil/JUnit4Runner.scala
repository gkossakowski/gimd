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
package com.google.gimd.testingutil

import org.junit.runner.Runner
import org.scalacheck.Properties

/**
 * Simple JUnit4 Runner that runs ScalaCheck tests. 
 */
class JUnit4Runner(clazz: Class[Properties], seed: Long) extends Runner {
  import org.junit.runner.notification.RunNotifier
  import org.junit.runner.Description

  def this(clazz: Class[Properties]) = this(clazz, System.currentTimeMillis)

  lazy val props = clazz.newInstance

  lazy val description = descriptionFor(props)

  def run(notifier: RunNotifier) {
    import org.scalacheck.Test
    import Test._
    import org.junit.runner.notification.Failure
    def evalTest(name: String, result: Result) = {
      val desc = Description.createTestDescription(clazz, name)
      notifier.fireTestStarted(desc)
      result.status match {
        case Passed => notifier.fireTestFinished(desc)
        case Failed(args, labels) => {
          val e = new AssertionError("args %s, labels %s".format(args, labels))
          notifier.fireTestFailure(new Failure(desc, e))
        }
        case Exhausted => notifier.fireTestFailure(new Failure(desc, new AssertionError("Exhausted")))
        case PropException(_, e, _) => notifier.fireTestFailure(new Failure(desc, e))
        case GenException(e) => notifier.fireTestFailure(new Failure(desc, e))
      }
    }
    val rng = new java.util.Random(seed)
    val params = Test.defaultParams.copy(rng = rng)
    val rs: Seq[(String, Result)] = Test.checkProperties(props, params, (_, _, _) => (), evalTest)
    val failedRs = rs.filter { case (_, r) => !r.passed }
    if (!failedRs.isEmpty) {
      println("You can reproduce failed properties with seed %d".format(seed))
    }
  }

  def getDescription = description

  private def descriptionFor(props: Properties): Description = {
    val desc = Description.createSuiteDescription(props.name)
    props.properties foreach {
      case (name, _) => desc.addChild(Description.createTestDescription(clazz, name))
    }
    desc
  }



}
