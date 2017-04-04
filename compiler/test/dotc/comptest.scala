package dotc

import dotty.tools.dotc.vulpix.ParallelTesting

import scala.concurrent.duration._

object comptest extends ParallelTesting {

  def maxDuration = 3.seconds
  def numberOfSlaves = 5
  def safeMode = false
  def isInteractive = true
  def testFilter = None

  implicit val defaultOutputDir: String = "."

  val posDir = "./tests/pos/"
  val negDir = "./tests/neg/"
  val dotcDir = "./src/dotty/"

  def main(args: Array[String]): Unit =
    compileList(
      "comptest",
      List(
        dotcDir + "tools/dotc/CompilationUnit.scala",
        dotcDir + "tools/dotc/core/Types.scala",
        dotcDir + "tools/dotc/ast/Trees.scala"
      ),
      Array(
        "-Ylog:frontend",
        "-Xprompt"
      )
  )
}
