package dotc

import test._

object comptest extends CompilerTest {

  val posDir = "./tests/pos/"
  val negDir = "./tests/neg/"
  val dotcDir = "./src/dotty/"

  def main(args: Array[String]) =
    compileArgs(Array(
      dotcDir + "tools/dotc/CompilationUnit.scala",
      dotcDir + "tools/dotc/core/Types.scala",
      dotcDir + "tools/dotc/ast/Trees.scala",
      "#runs", "2",
      "-Ylog:frontend",
      "-Xprompt"))(Nil)

//    compileDir(dotcDir + "tools/dotc/printing", List("-Xprompt", "-Ylog:frontend", "#runs", "2", "-uniqid"))
}
