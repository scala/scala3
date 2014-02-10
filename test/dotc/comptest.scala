package dotc

import test._

object comptest extends CompilerTest {

  val posDir = "/Users/odersky/workspace/dotty/tests/pos/"
  val negDir = "/Users/odersky/workspace/dotty/tests/neg/"
  val dotcDir = "/Users/odersky/workspace/dotty/src/dotty/"

  def main(args: Array[String]) =
    compileDir(dotcDir + "tools/dotc/reporting", List("-Xprompt", "-Ylog:frontend", "#runs", "2"))
}