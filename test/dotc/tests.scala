package dotc

import org.junit.Test
import test._

class tests extends CompilerTest {

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes",
      "-Ycheck:tailrec,resolveSuper,mixin,restoreScopes",
      "-d", "./out/"
  )

  val doEmitBytecode = List("-Ystop-before:terminal")
  val failedbyName = List("-Ystop-before:collectEntryPoints") // #288
  val failedUnderscore = List("-Ystop-before:collectEntryPoints") // #289
  val testPickling = List("-Xprint-types", "-Ytest-pickler", "-Ystop-after:pickler")

  val failedOther = List("-Ystop-before:collectEntryPoints") // some non-obvious reason. need to look deeper
  val twice = List("#runs", "2", "-YnoDoubleBindings")
  val staleSymbolError: List[String] = List()

  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")

  val posDir = "./tests/pos/"
  val posSpecialDir = "./tests/pos-special/"
  val negDir = "./tests/neg/"
  val newDir = "./tests/new/"
  val dotcDir = "./src/dotty/"
  val picklingDir = "./tests/pickling"

  @Test def pickle_pickleOK = compileDir(picklingDir, testPickling)
  @Test def pickle_pickling = compileDir(dotcDir + "tools/dotc/core/pickling/", testPickling)

  //@Test def pickle_core = compileDir(dotcDir + "tools/dotc/core", testPickling, xerrors = 2) // two spurious comparison errors in Types and TypeOps

  @Test def pos_t2168_pat = compileFile(posDir, "t2168")
  @Test def pos_erasure = compileFile(posDir, "erasure")
  @Test def pos_Coder() = compileFile(posDir, "Coder")
  @Test def pos_blockescapes() = compileFile(posDir, "blockescapes")
  @Test def pos_collections() = compileFile(posDir, "collections")
  @Test def pos_functions1() = compileFile(posDir, "functions1")
  @Test def pos_implicits1() = compileFile(posDir, "implicits1")
  @Test def pos_inferred() = compileFile(posDir, "inferred")
  @Test def pos_Patterns() = compileFile(posDir, "Patterns")
  @Test def pos_selftypes() = compileFile(posDir, "selftypes")
  @Test def pos_varargs() = compileFile(posDir, "varargs")
  @Test def pos_vararg_patterns() = compileFile(posDir, "vararg-pattern")
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
  @Test def pos_overrides() = compileFile(posDir, "overrides")
  @Test def pos_javaOverride() = compileDir(posDir + "java-override")
  @Test def pos_templateParents() = compileFile(posDir, "templateParents")
  @Test def pos_overloadedAccess = compileFile(posDir, "overloadedAccess")
  @Test def pos_approximateUnion = compileFile(posDir, "approximateUnion")
  @Test def pos_tailcall = compileDir(posDir + "tailcall/")
  @Test def pos_nullarify = compileFile(posDir, "nullarify", "-Ycheck:nullarify" :: Nil)
  @Test def pos_subtyping = compileFile(posDir, "subtyping")
  @Test def pos_t2613 = compileFile(posSpecialDir, "t2613")(allowDeepSubtypes)
  @Test def pos_packageObj = compileFile(posDir, "i0239")
  @Test def pos_anonClassSubtyping = compileFile(posDir, "anonClassSubtyping")
  @Test def pos_extmethods = compileFile(posDir, "extmethods")

  @Test def pos_all = compileFiles(posDir)



  @Test def new_all = compileFiles(newDir, twice)

  @Test def neg_blockescapes() = compileFile(negDir, "blockescapesNeg", xerrors = 1)
  @Test def neg_typedapply() = compileFile(negDir, "typedapply", xerrors = 4)
  @Test def neg_typedidents() = compileFile(negDir, "typedIdents", xerrors = 2)
  @Test def neg_assignments() = compileFile(negDir, "assignments", xerrors = 3)
  @Test def neg_typers() = compileFile(negDir, "typers", xerrors = 12)
  @Test def neg_privates() = compileFile(negDir, "privates", xerrors = 2)
  @Test def neg_rootImports = compileFile(negDir, "rootImplicits", xerrors = 2)
  @Test def neg_templateParents() = compileFile(negDir, "templateParents", xerrors = 3)
  @Test def neg_autoTupling = compileFile(posDir, "autoTuplingTest", "-language:noAutoTupling" :: Nil, xerrors = 4)
  @Test def neg_autoTupling2 = compileFile(negDir, "autoTuplingTest", xerrors = 4)
  @Test def neg_companions = compileFile(negDir, "companions", xerrors = 1)
  @Test def neg_over = compileFile(negDir, "over", xerrors = 3)
  @Test def neg_overrides = compileFile(negDir, "overrides", xerrors = 11)
  @Test def neg_projections = compileFile(negDir, "projections", xerrors = 1)
  @Test def neg_i39 = compileFile(negDir, "i39", xerrors = 2)
  @Test def neg_i50_volatile = compileFile(negDir, "i50-volatile", xerrors = 6)
  @Test def neg_t0273_doubledefs = compileFile(negDir, "t0273", xerrors = 1)
  @Test def neg_zoo = compileFile(negDir, "zoo", xerrors = 12)
  @Test def neg_t1192_legalPrefix = compileFile(negDir, "t1192", xerrors = 1)
  @Test def neg_tailcall_t1672b = compileFile(negDir, "tailcall/t1672b", xerrors = 6)
  @Test def neg_tailcall_t3275 = compileFile(negDir, "tailcall/t3275", xerrors = 1)
  @Test def neg_tailcall_t6574 = compileFile(negDir, "tailcall/t6574", xerrors = 2)
  @Test def neg_tailcall = compileFile(negDir, "tailcall/tailrec", xerrors = 7)
  @Test def neg_tailcall2 = compileFile(negDir, "tailcall/tailrec-2", xerrors = 2)
  @Test def neg_tailcall3 = compileFile(negDir, "tailcall/tailrec-3", xerrors = 2)
  @Test def nef_t1279a = compileFile(negDir, "t1279a", xerrors = 1)
  @Test def neg_t1843_variances = compileFile(negDir, "t1843-variances", xerrors = 1)
  @Test def neg_t2660_ambi = compileFile(negDir, "t2660", xerrors = 2)
  @Test def neg_t2994 = compileFile(negDir, "t2994", xerrors = 2)
  @Test def neg_subtyping = compileFile(negDir, "subtyping", xerrors = 4)
  @Test def neg_variances = compileFile(negDir, "variances", xerrors = 2)
  @Test def neg_badAuxConstr = compileFile(negDir, "badAuxConstr", xerrors = 2)
  @Test def neg_typetest = compileFile(negDir, "typetest", xerrors = 1)
  @Test def neg_t1569_failedAvoid = compileFile(negDir, "t1569-failedAvoid", xerrors = 1)
  @Test def neg_cycles = compileFile(negDir, "cycles", xerrors = 8)
  @Test def neg_boundspropagation = compileFile(negDir, "boundspropagation", xerrors = 5)
  @Test def neg_refinedSubtyping = compileFile(negDir, "refinedSubtyping", xerrors = 2)
  @Test def neg_i0091_infpaths = compileFile(negDir, "i0091-infpaths", xerrors = 3)
  @Test def neg_i0248_inherit_refined = compileFile(negDir, "i0248-inherit-refined", xerrors = 4)
  @Test def neg_i0281 = compileFile(negDir, "i0281-null-primitive-conforms", xerrors = 3)
  @Test def neg_moduleSubtyping = compileFile(negDir, "moduleSubtyping", xerrors = 4)
  @Test def neg_escapingRefs = compileFile(negDir, "escapingRefs", xerrors = 2)

  @Test def dotc = compileDir(dotcDir + "tools/dotc", failedOther)(allowDeepSubtypes) // see dotc_core
  @Test def dotc_ast = compileDir(dotcDir + "tools/dotc/ast", failedOther)
    //similar to dotc_core_pickling but for another anon class. Still during firstTransform
  @Test def dotc_config = compileDir(dotcDir + "tools/dotc/config")
  @Test def dotc_core = compileDir(dotcDir + "tools/dotc/core", failedOther)(allowDeepSubtypes)
    // error: error while loading ConstraintHandling$$anon$1$,
    // class file 'target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar(dotty/tools/dotc/core/ConstraintHandling$$anon$1.class)'
    // has location not matching its contents: contains class $anon

  @Test def dotc_core_pickling = compileDir(dotcDir + "tools/dotc/core/pickling", failedOther)(allowDeepSubtypes)
    // exception caught when loading class ClassfileParser$$anon$1: dotty.tools.dotc.core.Denotations$NotDefinedHere:
    // demanding denotation of module class ClassfileParser$$anon$1$ at phase frontend(1) outside defined interval:
    // defined periods are Period(31..36, run = 2) Period(3..24, run = 2) Period(25..26, run = 2)
    // Period(27..28, run = 2) Period(29..29, run = 2) Period(30..30, run = 2)
    // inside FirstTransform 	at dotty.tools.dotc.transform.FirstTransform.transform(FirstTransform.scala:33)
    // weird.

  @Test def dotc_transform = compileDir(dotcDir + "tools/dotc/transform")

  @Test def dotc_parsing = compileDir(dotcDir + "tools/dotc/parsing")

  @Test def dotc_printing = compileDir(dotcDir + "tools/dotc/printing")

  @Test def dotc_reporting = compileDir(dotcDir + "tools/dotc/reporting", twice)

  @Test def dotc_typer = compileDir(dotcDir + "tools/dotc/typer", failedOther)
    // error: error while loading Checking$$anon$2$,
    // class file 'target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar(dotty/tools/dotc/typer/Checking$$anon$2.class)'
    // has location not matching its contents: contains class $anon

  @Test def dotc_util = compileDir(dotcDir + "tools/dotc/util", failedOther)
    // java.lang.ClassCastException: dotty.tools.dotc.core.Types$NoType$ cannot be cast to dotty.tools.dotc.core.Types$ClassInfo
    // at dotty.tools.dotc.core.SymDenotations$ClassDenotation.classInfo(SymDenotations.scala:1026)
    // at dotty.tools.dotc.transform.ExtensionMethods.transform(ExtensionMethods.scala:38)

  @Test def tools_io = compileDir(dotcDir + "tools/io", failedOther) // inner class has symbol <none>

  @Test def helloWorld = compileFile(posDir, "HelloWorld")
  @Test def labels = compileFile(posDir, "Labels")
  //@Test def tools = compileDir(dotcDir + "tools", "-deep" :: Nil)(allowDeepSubtypes)

  @Test def testNonCyclic = compileArgs(Array(
      dotcDir + "tools/dotc/CompilationUnit.scala",
      dotcDir + "tools/dotc/core/Types.scala",
      dotcDir + "tools/dotc/ast/Trees.scala",
      "-Xprompt"
      ) ++ staleSymbolError)

  @Test def testIssue_34 = compileArgs(Array(
      dotcDir + "tools/dotc/config/Properties.scala",
      dotcDir + "tools/dotc/config/PathResolver.scala",
      //"-Ylog:frontend",
      "-Xprompt") ++ staleSymbolError)

  val javaDir = "./tests/pos/java-interop/"
  @Test def java_all = compileFiles(javaDir)
  
  //@Test def dotc_compilercommand = compileFile(dotcDir + "tools/dotc/config/", "CompilerCommand")
}
