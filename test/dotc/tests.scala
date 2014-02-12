package dotc

import org.junit.Test
import test._

class tests extends CompilerTest {

  override val defaultOptions =
    List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "#runs", "2",
        "-pagewidth", "160")

  val posDir = "./tests/pos/"
  val negDir = "./tests/neg/"
  val dotcDir = "./src/dotty/"

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
  @Test def pos_typedidents() = compileFile(posDir, "typedIdents")
  @Test def pos_assignments() = compileFile(posDir, "assignments")
  @Test def pos_packageobject() = compileFile(posDir, "packageobject")
  @Test def pos_overloaded() = compileFile(posDir, "overloaded")

  @Test def neg_blockescapes() = compileFile(negDir, "blockescapesNeg", xerrors = 1)
  @Test def neg_typedapply() = compileFile(negDir, "typedapply", xerrors = 4)
  @Test def neg_typedidents() = compileFile(negDir, "typedIdents", xerrors = 2)
  @Test def neg_assignments() = compileFile(negDir, "assignments", xerrors = 3)
  @Test def neg_typers() = compileFile(negDir, "typers", xerrors = 10)
  @Test def neg_rootImports = compileFile(negDir, "rootImplicits", xerrors = 2)

  @Test def dotc = compileDir(dotcDir + "tools/dotc")
  @Test def dotc_ast = compileDir(dotcDir + "tools/dotc/ast")
  @Test def dotc_config = compileDir(dotcDir + "tools/dotc/config")
  @Test def dotc_core = compileDir(dotcDir + "tools/dotc/core")
  @Test def dotc_core_pickling = compileDir(dotcDir + "tools/dotc/core/pickling")
  @Test def dotc_transform = compileDir(dotcDir + "tools/dotc/core/transform")
  @Test def dotc_parsing = compileDir(dotcDir + "tools/dotc/parsing")
  @Test def dotc_printing = compileDir(dotcDir + "tools/dotc/printing")
  @Test def dotc_reporting = compileDir(dotcDir + "tools/dotc/reporting")
  @Test def dotc_typer = compileDir(dotcDir + "tools/dotc/typer")
  @Test def dotc_util = compileDir(dotcDir + "tools/dotc/util")
  @Test def tools_io = compileDir(dotcDir + "tools/io")
  @Test def tools = compileDir(dotcDir + "tools")

  @Test def testNonCyclic = compileArgs(Array(
      dotcDir + "tools/dotc/CompilationUnit.scala",
      dotcDir + "tools/dotc/core/Types.scala",
      dotcDir + "tools/dotc/ast/Trees.scala",
      "-Ylog:frontend",
      "-Xprompt"))

  //@Test def dotc_compilercommand = compileFile(dotcDir + "tools/dotc/config/", "CompilerCommand")
}
