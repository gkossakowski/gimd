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

import sbt._
import de.element34.sbteclipsify.Eclipsify

class GimdProject(info: ProjectInfo) extends DefaultProject(info) with IdeaPlugin with Eclipsify
  with PerformanceTests {
  val jgitRepo = "jgit-repository" at "http://download.eclipse.org/jgit/maven"
  val jgit = "org.eclipse.jgit" % "org.eclipse.jgit" % "0.7.1" withSources()

  val scalaTools = "scala-tools-snapshots" at "http://scala-tools.org/repo-snapshots"
  val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.0.RC3" % "1.8-SNAPSHOT" % "test"

  val bryanjswift = "Bryan J Swift Repository" at "http://repos.bryanjswift.com/maven2/"
  val junitInterface = "com.novocode" % "junit-interface" % "0.4.0" % "test"

  val junit = "junit" % "junit" % "4.8.1" % "test"
  val commonsIo = "commons-io" % "commons-io" % "1.3.2" % "test" withSources()
  val lucene = "org.apache.lucene" % "lucene-core" % "3.0.0" withSources()
}

/**
 * A trait that adds 'perf-test' task that executed performance tests.
 *
 * Performance tests are identified by having "PerfTest" suffix in their name.
 */
trait PerformanceTests extends BasicScalaProject {
  override def testOptions = TestFilter(!_.endsWith("PerfTest")) :: super.testOptions.toList
  val perfOptions = TestFilter(_ endsWith "PerfTest") :: TestListeners(testListeners) :: Nil
  lazy val perfTest = defaultTestTask(perfOptions) describedAs ("Runs all performance tests " +
          "detected during compilation.")
}
