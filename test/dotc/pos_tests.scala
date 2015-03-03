package dotc

import org.junit.Test
import test._

class pos_tests extends CompilerTest {

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes",
      "-Ycheck:resolveSuper,mixin,restoreScopes"
  )

  val twice = List("#runs", "2", "-YnoDoubleBindings")
  val doErase = List("-Ystop-before:terminal")
  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")

  val posDir = "./tests/pos/"
  val posSpecialDir = "./tests/pos-special/"
  val negDir = "./tests/neg/"
  val newDir = "./tests/new/"
  val dotcDir = "./src/dotty/"


  @Test def pos_t2168_pat = compileFile(posDir, "t2168", doErase)
  @Test def pos_erasure = compileFile(posDir, "erasure", doErase)
  @Test def pos_Coder() = compileFile(posDir, "Coder", doErase)
  @Test def pos_blockescapes() = compileFile(posDir, "blockescapes", doErase)
  @Test def pos_collections() = compileFile(posDir, "collections", doErase)
  @Test def pos_functions1() = compileFile(posDir, "functions1", doErase)
  @Test def pos_implicits1() = compileFile(posDir, "implicits1", doErase)
  @Test def pos_inferred() = compileFile(posDir, "inferred", doErase)
  @Test def pos_Patterns() = compileFile(posDir, "Patterns")
  @Test def pos_selftypes() = compileFile(posDir, "selftypes", doErase)
  @Test def pos_varargs() = compileFile(posDir, "varargs", doErase)
  @Test def pos_vararg_patterns() = compileFile(posDir, "vararg-pattern", doErase)
  @Test def pos_opassign() = compileFile(posDir, "opassign", doErase)
  @Test def pos_typedapply() = compileFile(posDir, "typedapply", doErase)
  @Test def pos_nameddefaults() = compileFile(posDir, "nameddefaults", doErase)
  @Test def pos_desugar() = compileFile(posDir, "desugar", doErase)
  @Test def pos_sigs() = compileFile(posDir, "sigs", doErase)
  @Test def pos_typers() = compileFile(posDir, "typers", doErase)
  @Test def pos_typedidents() = compileFile(posDir, "typedIdents", doErase)
  @Test def pos_assignments() = compileFile(posDir, "assignments", doErase)
  @Test def pos_packageobject() = compileFile(posDir, "packageobject", doErase)
  @Test def pos_overloaded() = compileFile(posDir, "overloaded", doErase)
  @Test def pos_overrides() = compileFile(posDir, "overrides", doErase)
  @Test def pos_templateParents() = compileFile(posDir, "templateParents", doErase)
  @Test def pos_structural() = compileFile(posDir, "structural", doErase)
  @Test def pos_overloadedAccess = compileFile(posDir, "overloadedAccess", doErase)
  @Test def pos_approximateUnion = compileFile(posDir, "approximateUnion", doErase)
  @Test def pos_onetwo = compileFile(posDir+"tailcall/", "t6479", doErase)
  @Test def pos_tailcall = compileDir(posDir + "tailcall/", doErase)
  @Test def pos_nullarify = compileFile(posDir, "nullarify", "-Ycheck:nullarify" :: doErase)
  @Test def pos_subtyping = compileFile(posDir, "subtyping", doErase)
  @Test def pos_t2613 = compileFile(posSpecialDir, "t2613", doErase)(allowDeepSubtypes)
}
