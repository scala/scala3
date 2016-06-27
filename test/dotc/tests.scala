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
      "-Yno-deep-subtypes", "-Yno-double-bindings", "-Yforce-sbt-phases",
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
  val posScala2Dir  = testsDir + "pos-scala2/"
  val negDir        = testsDir + "neg/"
  val runDir        = testsDir + "run/"
  val newDir        = testsDir + "new/"
  val replDir       = testsDir + "repl/"

  val sourceDir = "./src/"
  val dottyDir  = sourceDir + "dotty/"
  val toolsDir  = dottyDir + "tools/"
  val backendDir = toolsDir + "backend/"
  val dotcDir   = toolsDir + "dotc/"
  val coreDir   = dotcDir + "core/"
  val parsingDir = dotcDir + "parsing/"
  val dottyReplDir   = dotcDir + "repl/"
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

  @Test def pos_scala2_all = compileFiles(posScala2Dir, scala2mode)

  @Test def rewrites = compileFile(posScala2Dir, "rewrites", "-rewrite" :: scala2mode)

  @Test def pos_859 = compileFile(posSpecialDir, "i859", scala2mode)(allowDeepSubtypes)

  @Test def new_all = compileFiles(newDir, twice)
  @Test def repl_all = replFiles(replDir)

  @Test def neg_all = compileFiles(negDir, verbose = true, compileSubDirs = false)
  @Test def neg_typedIdents() = compileDir(negDir, "typedIdents")

  val negCustomArgs = negDir + "customArgs/"

  @Test def neg_cli_error = compileFile(negCustomArgs, "cliError", List("-thisOptionDoesNotExist"))

  @Test def neg_typers() = compileFile(negCustomArgs, "typers")(allowDoubleBindings)
  @Test def neg_overrideClass = compileFile(negCustomArgs, "overrideClass", scala2mode)
  @Test def neg_autoTupling = compileFile(negCustomArgs, "autoTuplingTest", args = "-language:noAutoTupling" :: Nil)
  @Test def neg_i1050 = compileFile(negCustomArgs, "i1050", List("-strict"))
  @Test def neg_i1240 = compileFile(negCustomArgs, "i1240")(allowDoubleBindings)

  val negTailcallDir = negDir + "tailcall/"
  @Test def neg_tailcall_t1672b = compileFile(negTailcallDir, "t1672b")
  @Test def neg_tailcall_t3275 = compileFile(negTailcallDir, "t3275")
  @Test def neg_tailcall_t6574 = compileFile(negTailcallDir, "t6574")
  @Test def neg_tailcall = compileFile(negTailcallDir, "tailrec")
  @Test def neg_tailcall2 = compileFile(negTailcallDir, "tailrec-2")
  @Test def neg_tailcall3 = compileFile(negTailcallDir, "tailrec-3")

  @Test def neg_nopredef = compileFile(negCustomArgs, "nopredef", List("-Yno-predef"))
  @Test def neg_noimports = compileFile(negCustomArgs, "noimports", List("-Yno-imports"))
  @Test def neg_noimpots2 = compileFile(negCustomArgs, "noimports2", List("-Yno-imports"))

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

  @Test def dotty = compileDir(dottyDir, ".", List("-deep", "-Ycheck-reentrant", "-strict"))(allowDeepSubtypes) // note the -deep argument

  @Test def dotc_ast = compileDir(dotcDir, "ast")
  @Test def dotc_config = compileDir(dotcDir, "config")
  @Test def dotc_core = compileDir(dotcDir, "core")(allowDeepSubtypes)// twice omitted to make tests run faster
  @Test def dotc_core_nocheck = compileDir(dotcDir, "core")(noCheckOptions)

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

  //TASTY tests
  @Test def tasty_new_all = compileFiles(newDir, testPickling)

  @Test def tasty_dotty = compileDir(sourceDir, "dotty", testPickling)

  // Disabled because we get stale symbol errors on the SourceFile annotation, which is normal.
  // @Test def tasty_annotation_internal = compileDir(s"${dottyDir}annotation/", "internal", testPickling)

  @Test def tasty_runtime = compileDir(s"$dottyDir", "runtime", testPickling)
  @Test def tasty_runtime_vc = compileDir(s"${dottyDir}runtime/", "vc", testPickling)

  @Test def tasty_tools = compileDir(dottyDir, "tools", testPickling)

  //TODO: issue with ./src/dotty/tools/backend/jvm/DottyBackendInterface.scala
  @Test def tasty_backend_jvm = compileList("tasty_backend_jvm", List(
    "CollectEntryPoints.scala", "GenBCode.scala", "LabelDefs.scala",
    "scalaPrimitives.scala"
  ) map (s"${backendDir}jvm/" + _), testPickling)

  @Test def tasty_backend_sjs = compileDir(s"${backendDir}", "sjs", testPickling)

  @Test def tasty_dotc = compileDir(toolsDir, "dotc", testPickling)
  @Test def tasty_dotc_ast = compileDir(dotcDir, "ast", testPickling)
  @Test def tasty_dotc_config = compileDir(dotcDir, "config", testPickling)

  //TODO: issue with ./src/dotty/tools/dotc/core/Types.scala
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

  @Test def tasty_classfile = compileDir(coreDir, "classfile", testPickling)
  @Test def tasty_tasty = compileDir(coreDir, "tasty", testPickling)
  @Test def tasty_unpickleScala2 = compileDir(coreDir, "unpickleScala2", testPickling)

  //TODO: issue with ./src/dotty/tools/dotc/parsing/Parsers.scala
  @Test def tasty_dotc_parsing = compileList("tasty_dotc_parsing", List(
    "CharArrayReader.scala", "JavaParsers.scala", "JavaScanners.scala", "JavaTokens.scala",
    "MarkupParserCommon.scala", "MarkupParsers.scala", "package.scala" ,"Scanners.scala",
    "ScriptParsers.scala", "SymbolicXMLBuilder.scala", "Tokens.scala", "Utility.scala"
  ) map (parsingDir + _), testPickling)

  @Test def tasty_dotc_printing = compileDir(dotcDir, "printing", testPickling)

  @Test def tasty_dotc_repl = compileDir(dotcDir, "repl", testPickling)

  //@Test def tasty_dotc_reporting = compileDir(dotcDir, "reporting", testPickling)
  @Test def tasty_dotc_rewrite = compileDir(dotcDir, "rewrite", testPickling)

  //TODO: issues with LazyVals.scala, PatternMatcher.scala
  @Test def tasty_dotc_transform = compileList("tasty_dotc_transform", List(
    "AugmentScala2Traits.scala", "CapturedVars.scala", "CheckReentrant.scala", "CheckStatic.scala",
    "ClassOf.scala", "CollectEntryPoints.scala", "Constructors.scala", "CrossCastAnd.scala",
    "CtxLazy.scala", "ElimByName.scala", "ElimErasedValueType.scala", "ElimRepeated.scala",
    "ElimStaticThis.scala", "Erasure.scala", "ExpandPrivate.scala", "ExpandSAMs.scala",
    "ExplicitOuter.scala", "ExplicitSelf.scala", "ExtensionMethods.scala", "FirstTransform.scala",
    "Flatten.scala", "FullParameterization.scala", "FunctionalInterfaces.scala", "GetClass.scala",
    "Getters.scala", "InterceptedMethods.scala", "LambdaLift.scala", "LiftTry.scala", "LinkScala2ImplClasses.scala",
    "MacroTransform.scala", "Memoize.scala", "Mixin.scala", "MixinOps.scala", "NonLocalReturns.scala",
    "NormalizeFlags.scala", "OverridingPairs.scala", "ParamForwarding.scala", "Pickler.scala", "PostTyper.scala",
    "ResolveSuper.scala", "RestoreScopes.scala", "SeqLiterals.scala", "Splitter.scala", "SuperAccessors.scala",
    "SymUtils.scala", "SyntheticMethods.scala", "TailRec.scala", "TreeChecker.scala", "TreeExtractors.scala",
    "TreeGen.scala", "TreeTransform.scala", "TypeTestsCasts.scala", "TypeUtils.scala", "ValueClasses.scala",
    "VCElideAllocations.scala", "VCInlineMethods.scala"
  ) map (s"${dotcDir}transform/" + _), testPickling)

  //TODO: issue with ./src/dotty/tools/dotc/typer/Namer.scala
  @Test def tasty_typer = compileList("tasty_typer", List(
    "Applications.scala", "Checking.scala", "ConstFold.scala", "ErrorReporting.scala",
    "EtaExpansion.scala", "FrontEnd.scala", "Implicits.scala", "ImportInfo.scala",
    "Inferencing.scala", "ProtoTypes.scala", "ReTyper.scala", "RefChecks.scala",
    "TypeAssigner.scala", "Typer.scala", "VarianceChecker.scala", "Variances.scala"
  ) map (typerDir + _), testPickling)

  @Test def tasty_dotc_util = compileDir(dotcDir, "util", testPickling)
  @Test def tasty_tools_io = compileDir(toolsDir, "io", testPickling)
  @Test def tasty_tests = compileDir(testsDir, "tasty", testPickling)
}
