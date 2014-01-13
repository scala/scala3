package dotc

import org.junit.Test
import test._

class tests extends CompilerTest {

  override val defaultOptions =
    List(
//        "-verbose",
 //         "-Ylog:frontend",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

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

  @Test def neg_blockescapes() = compileFile(negDir, "blockescapesNeg", xerrors = 1)
  @Test def neg_typedapply() = compileFile(negDir, "typedapply", xerrors = 4)
  @Test def neg_typedidents() = compileFile(negDir, "typedidents", xerrors = 2)
  @Test def neg_assignments() = compileFile(negDir, "assignments", xerrors = 3)
  @Test def neg_typers() = compileFile(negDir, "typers", xerrors = 10)

  @Test def dotc = compileDir(dotcDir + "tools/dotc")
  @Test def dotc_ast = compileDir(dotcDir + "tools/dotc/ast")
  @Test def dotc_config = compileDir(dotcDir + "tools/dotc/config")
  @Test def dotc1 = compileFile(dotcDir + "tools/dotc/core/", "Annotations")
  @Test def dotc2 = compileFile(dotcDir + "tools/dotc/core/", "Constants")
  @Test def dotc3 = compileFile(dotcDir + "tools/dotc/core/", "Constraint")
  @Test def dotc4 = compileFile(dotcDir + "tools/dotc/core/", "Contexts")
  @Test def dotc5 = compileFile(dotcDir + "tools/dotc/core/", "Decorators")
  @Test def dotc6 = compileFile(dotcDir + "tools/dotc/core/", "Definitions")
  @Test def dotc7 = compileFile(dotcDir + "tools/dotc/core/", "Denotations")
  @Test def dotc8 = compileFile(dotcDir + "tools/dotc/core/", "DotClass")
  @Test def dotc9 = compileFile(dotcDir + "tools/dotc/core/", "Flags")
  @Test def dotc10 = compileFile(dotcDir + "tools/dotc/core/", "NameOps")
  @Test def dotc11 = compileFile(dotcDir + "tools/dotc/core/", "Names")
  @Test def dotc12 = compileFile(dotcDir + "tools/dotc/core/", "Periods")
  @Test def dotc13 = compileFile(dotcDir + "tools/dotc/core/", "Phases")
  @Test def dotc14 = compileFile(dotcDir + "tools/dotc/core/", "Scopes")
  @Test def dotc15 = compileFile(dotcDir + "tools/dotc/core/", "Signature")
  @Test def dotc16 = compileFile(dotcDir + "tools/dotc/core/", "StdNames")
  @Test def dotc17 = compileFile(dotcDir + "tools/dotc/core/", "Substituters")
  @Test def dotc18 = compileFile(dotcDir + "tools/dotc/core/", "SymbolLoaders")
  @Test def dotc19 = compileFile(dotcDir + "tools/dotc/core/", "Symbols")
  @Test def dotc20 = compileFile(dotcDir + "tools/dotc/core/", "SymDenotations")



//  @Test def dotc_compilercommand = compileFile(dotcDir + "tools/dotc/config/", "CompilerCommand")

}