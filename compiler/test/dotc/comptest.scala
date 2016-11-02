package dotc

import dotty.tools.dotc.CompilerTest

object comptest extends CompilerTest {

  override val generatePartestFiles = false
  val defaultOutputDir: String = ""

  val posDir = "./tests/pos/"
  val negDir = "./tests/neg/"
  val dotcDir = "./src/dotty/"

  def main(args: Array[String]) =
    compileList("comptest", List(
      dotcDir + "tools/dotc/CompilationUnit.scala",
      dotcDir + "tools/dotc/core/Types.scala",
      dotcDir + "tools/dotc/ast/Trees.scala"), List(
      "#runs", "2",
      "-Ylog:frontend",
      "-Xprompt"))(Nil)

//    compileDir(dotcDir + "tools/dotc/", "printing", List("-Xprompt", "-Ylog:frontend", "#runs", "2", "-uniqid"))
}
