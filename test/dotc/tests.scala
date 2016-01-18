package dotc

import test._
import dotty.partest._
import org.junit.Test
import org.junit.experimental.categories._

import scala.io.Source

// tests that match regex '(pos|dotc|run|java|compileStdLib)\.*' would be executed as benchmarks.
class tests extends CompilerTest {

  def isRunByJenkins: Boolean = sys.props.isDefinedAt("dotty.jenkins.build")

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  val defaultOutputDir = "./out/"

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes", "-Yno-double-bindings",
      "-d", defaultOutputDir) ++ {
    if (isRunByJenkins) List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef") // should be Ycheck:all, but #725
    else List("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef")
  }


  val testPickling = List("-Xprint-types", "-Ytest-pickler", "-Ystop-after:pickler")

  val twice = List("#runs", "2")
  val staleSymbolError: List[String] = List()

  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")
  val allowDoubleBindings = defaultOptions diff List("-Yno-double-bindings")
  val scala2mode = List("-language:Scala2")

  val testsDir      = "./tests/"
  val posDir        = testsDir + "pos/"
  val posSpecialDir = testsDir + "pos-special/"
  val negDir        = testsDir + "neg/"
  val runDir        = testsDir + "run/"
  val newDir        = testsDir + "new/"

  val sourceDir = "./src/"
  val dottyDir  = sourceDir + "dotty/"
  val toolsDir  = dottyDir + "tools/"
  val dotcDir   = toolsDir + "dotc/"
  val coreDir   = dotcDir + "core/"
  val typerDir  = dotcDir + "typer/"

  @Test def pickle_pickleOK = compileDir(testsDir, "pickling", testPickling)
// This directory doesn't exist anymore
// @Test def pickle_pickling = compileDir(coreDir, "pickling", testPickling)
  @Test def pickle_ast = compileDir(dotcDir, "ast", testPickling)
  @Test def pickle_inf = compileFile(posDir, "pickleinf", testPickling)

  //@Test def pickle_core = compileDir(dotcDir, "core", testPickling, xerrors = 2) // two spurious comparison errors in Types and TypeOps

  @Test def pos_arraycopy =
    compileFile(runDir, "arraycopy", List("-Ylog-classpath"))
  @Test def pos_t2168_pat = compileFile(posDir, "t2168", twice)
  @Test def pos_erasure = compileFile(posDir, "erasure", twice)
  @Test def pos_Coder() = compileFile(posDir, "Coder", twice)
  @Test def pos_blockescapes() = compileFile(posDir, "blockescapes", twice)
  @Test def pos_collections() = compileFile(posDir, "collections", twice)
  @Test def pos_functions1() = compileFile(posDir, "functions1", twice)
  @Test def pos_implicits1() = compileFile(posDir, "implicits1", twice)
  @Test def pos_inferred() = compileFile(posDir, "inferred", twice)
  @Test def pos_Patterns() = compileFile(posDir, "Patterns", twice)
  @Test def pos_selftypes() = compileFile(posDir, "selftypes", twice)
  @Test def pos_varargs() = compileFile(posDir, "varargs", twice)
  @Test def pos_vararg_patterns() = compileFile(posDir, "vararg-pattern", twice)
  @Test def pos_opassign() = compileFile(posDir, "opassign", twice)
  @Test def pos_typedapply() = compileFile(posDir, "typedapply", twice)
  @Test def pos_nameddefaults() = compileFile(posDir, "nameddefaults", twice)
  @Test def pos_desugar() = compileFile(posDir, "desugar", twice)
  @Test def pos_sigs() = compileFile(posDir, "sigs", twice)
  @Test def pos_typers() = compileFile(posDir, "typers", twice)
  @Test def pos_typedIdents() = compileDir(posDir, "typedIdents", twice)
  @Test def pos_assignments() = compileFile(posDir, "assignments", twice)
  @Test def pos_packageobject() = compileFile(posDir, "packageobject", twice)
  @Test def pos_overloaded() = compileFile(posDir, "overloaded", twice)
  @Test def pos_overrides() = compileFile(posDir, "overrides", twice)
  @Test def pos_javaOverride() = compileDir(posDir, "java-override", twice)
  @Test def pos_templateParents() = compileFile(posDir, "templateParents", twice)
  @Test def pos_overloadedAccess = compileFile(posDir, "overloadedAccess", twice)
  @Test def pos_approximateUnion = compileFile(posDir, "approximateUnion", twice)
  @Test def pos_tailcall = compileDir(posDir, "tailcall", twice)
  @Test def pos_valueclasses = compileFiles(posDir + "valueclasses/", twice)
  @Test def pos_nullarify = compileFile(posDir, "nullarify", args = "-Ycheck:nullarify" :: Nil)
  @Test def pos_subtyping = compileFile(posDir, "subtyping", twice)
  @Test def pos_packageObj = compileFile(posDir, "i0239", twice)
  @Test def pos_anonClassSubtyping = compileFile(posDir, "anonClassSubtyping", twice)
  @Test def pos_extmethods = compileFile(posDir, "extmethods", twice)
  @Test def pos_companions = compileFile(posDir, "companions", twice)

  @Test def pos_all = compileFiles(posDir) // twice omitted to make tests run faster

  @Test def pos_i871 = compileFile(posSpecialDir, "i871", scala2mode)
  @Test def pos_variancesConstr = compileFile(posSpecialDir, "variances-constr", scala2mode)

  @Test def new_all = compileFiles(newDir, twice)

  @Test def neg_abstractOverride() = compileFile(negDir, "abstract-override", xerrors = 2)
  @Test def neg_blockescapes() = compileFile(negDir, "blockescapesNeg", xerrors = 1)
  @Test def neg_bounds() = compileFile(negDir, "bounds", xerrors = 3)
  @Test def neg_typedapply() = compileFile(negDir, "typedapply", xerrors = 3)
  @Test def neg_typedIdents() = compileDir(negDir, "typedIdents", xerrors = 2)
  @Test def neg_assignments() = compileFile(negDir, "assignments", xerrors = 3)
  @Test def neg_typers() = compileFile(negDir, "typers", xerrors = 14)(allowDoubleBindings)
  @Test def neg_privates() = compileFile(negDir, "privates", xerrors = 2)
  @Test def neg_rootImports = compileFile(negDir, "rootImplicits", xerrors = 2)
  @Test def neg_templateParents() = compileFile(negDir, "templateParents", xerrors = 3)
  @Test def neg_autoTupling = compileFile(posDir, "autoTuplingTest", args = "-language:noAutoTupling" :: Nil, xerrors = 3)
  @Test def neg_autoTupling2 = compileFile(negDir, "autoTuplingTest", xerrors = 3)
  @Test def neg_companions = compileFile(negDir, "companions", xerrors = 1)
  @Test def neg_over = compileFile(negDir, "over", xerrors = 3)
  @Test def neg_overrides = compileFile(negDir, "overrides", xerrors = 12)
  @Test def neg_overrideClass = compileFile(negDir, "overrideClass", List("-language:Scala2"), xerrors = 1)
  @Test def neg_i39 = compileFile(negDir, "i39", xerrors = 2)
  @Test def neg_i50_volatile = compileFile(negDir, "i50-volatile", xerrors = 6)
  @Test def neg_zoo = compileFile(negDir, "zoo", xerrors = 12)

  val negTailcallDir = negDir + "tailcall/"
  @Test def neg_tailcall_t1672b = compileFile(negTailcallDir, "t1672b", xerrors = 5)
  @Test def neg_tailcall_t3275 = compileFile(negTailcallDir, "t3275", xerrors = 1)
  @Test def neg_tailcall_t6574 = compileFile(negTailcallDir, "t6574", xerrors = 2)
  @Test def neg_tailcall = compileFile(negTailcallDir, "tailrec", xerrors = 7)
  @Test def neg_tailcall2 = compileFile(negTailcallDir, "tailrec-2", xerrors = 2)
  @Test def neg_tailcall3 = compileFile(negTailcallDir, "tailrec-3", xerrors = 2)

  @Test def neg_t1843_variances = compileFile(negDir, "t1843-variances", xerrors = 1)
  @Test def neg_t2660_ambi = compileFile(negDir, "t2660", xerrors = 2)
  @Test def neg_t2994 = compileFile(negDir, "t2994", xerrors = 2)
  @Test def neg_subtyping = compileFile(negDir, "subtyping", xerrors = 5)
  @Test def neg_variances = compileFile(negDir, "variances", xerrors = 2)
  @Test def neg_variancesConstr = compileFile(negDir, "variances-constr", xerrors = 2)
  @Test def neg_i871_missingReturnType = compileFile(negDir, "i871", xerrors = 2)
  @Test def neg_badAuxConstr = compileFile(negDir, "badAuxConstr", xerrors = 2)
  @Test def neg_typetest = compileFile(negDir, "typetest", xerrors = 1)
  @Test def neg_t1569_failedAvoid = compileFile(negDir, "t1569-failedAvoid", xerrors = 1)
  @Test def neg_clashes = compileFile(negDir, "clashes", xerrors = 2)
  @Test def neg_cycles = compileFile(negDir, "cycles", xerrors = 8)
  @Test def neg_boundspropagation = compileFile(negDir, "boundspropagation", xerrors = 5)
  @Test def neg_refinedSubtyping = compileFile(negDir, "refinedSubtyping", xerrors = 2)
  @Test def neg_hklower = compileFile(negDir, "hklower", xerrors = 3)
  @Test def neg_Iter2 = compileFile(negDir, "Iter2", xerrors = 2)
  @Test def neg_i0091_infpaths = compileFile(negDir, "i0091-infpaths", xerrors = 3)
  @Test def neg_i0248_inherit_refined = compileFile(negDir, "i0248-inherit-refined", xerrors = 4)
  @Test def neg_i0281 = compileFile(negDir, "i0281-null-primitive-conforms", xerrors = 3)
  @Test def neg_i583 = compileFile(negDir, "i0583-skolemize", xerrors = 2)
  @Test def neg_i941 = compileFile(negDir, "i941", xerrors = 3)
  @Test def neg_finalSealed = compileFile(negDir, "final-sealed", xerrors = 2)
  @Test def neg_i705 = compileFile(negDir, "i705-inner-value-class", xerrors = 7)
  @Test def neg_i866 = compileFile(negDir, "i866", xerrors = 2)
  @Test def neg_i974 = compileFile(negDir, "i974", xerrors = 2)
  @Test def neg_moduleSubtyping = compileFile(negDir, "moduleSubtyping", xerrors = 4)
  @Test def neg_escapingRefs = compileFile(negDir, "escapingRefs", xerrors = 2)
  @Test def neg_instantiateAbstract = compileFile(negDir, "instantiateAbstract", xerrors = 8)
  @Test def neg_partialApplications = compileFile(negDir, "partialApplications", xerrors = 3)
  @Test def neg_selfInheritance = compileFile(negDir, "selfInheritance", xerrors = 6)
  @Test def neg_selfreq = compileFile(negDir, "selfreq", xerrors = 2)
  @Test def neg_singletons = compileFile(negDir, "singletons", xerrors = 8)
  @Test def neg_shadowedImplicits = compileFile(negDir, "arrayclone-new", xerrors = 2)
  @Test def neg_ski = compileFile(negDir, "ski", xerrors = 2)
  @Test def neg_traitParamsTyper = compileFile(negDir, "traitParamsTyper", xerrors = 5)
  @Test def neg_traitParamsMixin = compileFile(negDir, "traitParamsMixin", xerrors = 2)
  @Test def neg_firstError = compileFile(negDir, "firstError", xerrors = 3)
  @Test def neg_implicitLowerBound = compileFile(negDir, "implicit-lower-bound", xerrors = 1)
  @Test def neg_validate = compileFile(negDir, "validate", xerrors = 18)
  @Test def neg_validateParsing = compileFile(negDir, "validate-parsing", xerrors = 7)
  @Test def neg_validateRefchecks = compileFile(negDir, "validate-refchecks", xerrors = 2)
  @Test def neg_skolemize = compileFile(negDir, "skolemize", xerrors = 2)
  @Test def neg_nested_bounds = compileFile(negDir, "nested_bounds", xerrors = 1)

  @Test def run_all = runFiles(runDir)

  val stdlibFiles = Source.fromFile("./test/dotc/scala-collections.whitelist", "UTF8").getLines()
   .map(_.trim) // allow identation
   .filter(!_.startsWith("#")) // allow comment lines prefixed by #
   .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
   .filter(_.nonEmpty)
   .toList

  @Test def compileStdLib = compileList("compileStdLib", stdlibFiles, "-migration" :: scala2mode)
  @Test def compileMixed = compileLine(
      """tests/pos/B.scala
        |./scala-scala/src/library/scala/collection/immutable/Seq.scala
        |./scala-scala/src/library/scala/package.scala
        |./scala-scala/src/library/scala/collection/GenSeqLike.scala
        |./scala-scala/src/library/scala/collection/SeqLike.scala
        |./scala-scala/src/library/scala/collection/generic/GenSeqFactory.scala""".stripMargin)
  @Test def compileIndexedSeq = compileLine("./scala-scala/src/library/scala/collection/immutable/IndexedSeq.scala")

  @Test def dotty = compileDir(dottyDir, ".", List("-deep", "-Ycheck-reentrant"))(allowDeepSubtypes) // note the -deep argument

  @Test def dotc_ast = compileDir(dotcDir, "ast")
  @Test def dotc_config = compileDir(dotcDir, "config")
  @Test def dotc_core = compileDir(dotcDir, "core")("-Yno-double-bindings" :: allowDeepSubtypes)// twice omitted to make tests run faster

// This directory doesn't exist anymore
//  @Test def dotc_core_pickling = compileDir(coreDir, "pickling")(allowDeepSubtypes)// twice omitted to make tests run faster

  @Test def dotc_transform = compileDir(dotcDir, "transform")(allowDeepSubtypes)// twice omitted to make tests run faster

  @Test def dotc_parsing = compileDir(dotcDir, "parsing") // twice omitted to make tests run faster

  @Test def dotc_printing = compileDir(dotcDir, "printing") // twice omitted to make tests run faster

  @Test def dotc_reporting = compileDir(dotcDir, "reporting") // twice omitted to make tests run faster

  @Test def dotc_typer = compileDir(dotcDir, "typer")// twice omitted to make tests run faster
    // error: error while loading Checking$$anon$2$,
    // class file 'target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar(dotty/tools/dotc/typer/Checking$$anon$2.class)'
    // has location not matching its contents: contains class $anon

  @Test def dotc_util = compileDir(dotcDir, "util") // twice omitted to make tests run faster

  @Test def tools_io = compileDir(toolsDir, "io") // inner class has symbol <none>

  @Test def helloWorld = compileFile(posDir, "HelloWorld")
  @Test def labels = compileFile(posDir, "Labels", twice)
  //@Test def tools = compileDir(dottyDir, "tools", "-deep" :: Nil)(allowDeepSubtypes)

  @Test def testNonCyclic = compileList("testNonCyclic", List(
      dotcDir + "CompilationUnit.scala",
      coreDir + "Types.scala",
      dotcDir + "ast/Trees.scala"
    ), List("-Xprompt") ++ staleSymbolError ++ twice)

  @Test def testIssue_34 = compileList("testIssue_34", List(
      dotcDir + "config/Properties.scala",
      dotcDir + "config/PathResolver.scala"
    ), List(/* "-Ylog:frontend", */ "-Xprompt") ++ staleSymbolError ++ twice)

  val javaDir = "./tests/pos/java-interop/"
  @Test def java_all = compileFiles(javaDir, twice)
  //@Test def dotc_compilercommand = compileFile(dotcDir + "config/", "CompilerCommand")

  @Test def tasty_new_all = compileFiles(newDir, testPickling)
  @Test def tasty_dotc_config = compileDir(dotcDir, "config", testPickling)
  @Test def tasty_dotc_printing = compileDir(dotcDir, "printing", testPickling)
  //@Test def tasty_dotc_reporting = compileDir(dotcDir, "reporting", testPickling)
  @Test def tasty_dotc_util = compileDir(dotcDir, "util", testPickling)
  @Test def tasty_core = compileList("tasty_core", List(
      "Annotations.scala", "Constants.scala", "Constraint.scala", "ConstraintHandling.scala",
      "ConstraintRunInfo.scala", "Contexts.scala", "Decorators.scala", "Definitions.scala",
      "DenotTransformers.scala", "Denotations.scala", "Flags.scala", "Hashable.scala",
      "NameOps.scala", "Names.scala", "OrderingConstraint.scala", "Periods.scala",
      "Phases.scala", "Scopes.scala", "Signature.scala", "StdNames.scala",
      "Substituters.scala", "SymDenotations.scala", "SymbolLoaders.scala", "Symbols.scala",
      "TypeApplications.scala", "TypeComparer.scala", "TypeErasure.scala", "TypeOps.scala",
      "TyperState.scala", "Uniques.scala"
    ) map (coreDir + _), testPickling)
  @Test def tasty_typer = compileList("tasty_typer", List(
      "Applications.scala", "Checking.scala", "ConstFold.scala", "ErrorReporting.scala",
      "EtaExpansion.scala", "FrontEnd.scala", "Implicits.scala", "ImportInfo.scala",
      "Inferencing.scala", "Mode.scala", "ProtoTypes.scala", "ReTyper.scala", "RefChecks.scala",
      "TypeAssigner.scala", "Typer.scala", "VarianceChecker.scala", "Variances.scala"
    ) map (typerDir + _), testPickling)
  @Test def tasty_tasty = compileDir(coreDir, "tasty", testPickling)
  @Test def tasty_classfile = compileDir(coreDir, "classfile", testPickling)
  @Test def tasty_unpickleScala2 = compileDir(coreDir, "unpickleScala2", testPickling)
  @Test def tasty_tools_io = compileDir(toolsDir, "io", testPickling)
  @Test def tasty_tests = compileDir(testsDir, "tasty", testPickling)
}
