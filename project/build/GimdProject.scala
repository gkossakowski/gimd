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

class GimdProject(info: ProjectInfo) extends DefaultProject(info) {
  val jgitRepo = "jgit-repository" at "http://egit.googlecode.com/svn/maven/"
  val scalaTools = "scala-tools" at "http://scala-tools.org/repo-releases"
  val junit = "junit" % "junit" % "4.6"
  val jgit = "org.spearce" % "jgit" % "0.5.0-93-g5b89a2c"
  val commonsIo = "commons-io" % "commons-io" % "1.3.2"
  val scalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.0.RC1" % "1.7"
}

