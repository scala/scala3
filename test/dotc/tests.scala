package dotc

import org.junit.Test
import test._

class tests extends CompilerTest {

  override val defaultOptions =
    List("-verbose", "-Ylog:frontend", "-explaintypes", "-Yshow-suppressed-errors", "-pagewidth", "160")

  val posDir = "/Users/odersky/workspace/dotty/tests/pos/"
  val negDir = "/Users/odersky/workspace/dotty/tests/neg/"
  val dotcDir = "/Users/odersky/workspace/dotty/src/dotty/"

  @Test def pos_Coder() = compileFile(posDir, "Coder")
  @Test def pos_blockescapes() = compileFile(posDir, "blockescapes")
  @Test def pos_collections() = compileFile(posDir, "collections")
  @Test def pos_functions1() = compileFile(posDir, "functions1")
  @Test def pos_implicits1() = compileFile(posDir, "implicits1")
  @Test def pos_inferred() = compileFile(posDir, "inferred")
  @Test def pos_Patterns() = compileFile(posDir, "Patterns")
  @Test def pos_selftypes() = compileFile(posDir, "selftypes")
  @Test def pos_varargs() = compileFile(posDir, "varargs")
  @Test def pos_opassign() = compileFile(posDir, "opassign")
  @Test def pos_typedapply() = compileFile(posDir, "typedapply")
  @Test def pos_nameddefaults() = compileFile(posDir, "nameddefaults")
  @Test def pos_desugar() = compileFile(posDir, "desugar")
  @Test def pos_sigs() = compileFile(posDir, "sigs")
  @Test def pos_typers() = compileFile(posDir, "typers")
  @Test def pos_typedidents() = compileFile(posDir, "typedidents")
  @Test def pos_assignments() = compileFile(posDir, "assignments")
  @Test def pos_packageobject() = compileFile(posDir, "packageobject")
  @Test def pos_overloaded() = compileFile(posDir, "overloaded")

  @Test def neg_blockescapes() = compileFile(negDir, "blockescapesNeg", xerrors = 2)
  @Test def neg_typedapply() = compileFile(negDir, "typedapply", xerrors = 4)
  @Test def neg_typedidents() = compileFile(negDir, "typedidents", xerrors = 2)
  @Test def neg_assignments() = compileFile(negDir, "assignments", xerrors = 3)
  @Test def neg_typers() = compileFile(negDir, "typers", xerrors = 10)

  @Test def dotc = compileDir(dotcDir + "tools/dotc")
}