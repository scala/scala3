package dotc

import dotty.tools.dotc.ParallelTesting

object comptest extends ParallelTesting {

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
