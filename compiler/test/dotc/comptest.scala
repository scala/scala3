package dotc

import dotty.tools.dotc.vulpix.ParallelTesting

object comptest extends ParallelTesting {

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
