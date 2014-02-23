package dotc

import test._

object comptest extends CompilerTest {

  val posDir = "/Users/odersky/workspace/dotty/tests/pos/"
  val negDir = "/Users/odersky/workspace/dotty/tests/neg/"
  val dotcDir = "/Users/odersky/workspace/dotty/src/dotty/"

  def main(args: Array[String]) =
    compileArgs(Array(
      dotcDir + "compiler/internal/CompilationUnit.scala",
      dotcDir + "compiler/internal/core/Types.scala",
      dotcDir + "compiler/internal/ast/Trees.scala",
      "#runs", "2",
      "-Ylog:frontend",
      "-Xprompt"))

//    compileDir(dotcDir + "compiler/internal/printing", List("-Xprompt", "-Ylog:frontend", "#runs", "2", "-uniqid"))
}
