package dotty
package tools
package dotc

import scala.language.unsafeNulls

import org.junit.{Test, AfterClass}
import org.junit.Assume.*

import java.nio.file.*
import scala.concurrent.duration.*

import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.vulpix.*

class CompilationTests {
  import ParallelTesting.*
  import TestConfiguration.*
  import CompilationTests.{*, given}
  import CompilationTest.aggregateTests

  // Positive tests ------------------------------------------------------------

  @Test def pos: Unit = {
    given TestGroup = TestGroup("compilePos")
    val tests = List(
      compileFilesInDir("tests/pos", defaultOptions.and("-Wsafe-init", "-Wunused:all", "-Wshadow:private-shadow", "-Wshadow:type-parameter-shadow"), FileFilter.include(TestSources.posLintingAllowlist)),
      compileFilesInDir("tests/pos", defaultOptions.and("-Wsafe-init"), FileFilter.exclude(TestSources.posLintingAllowlist)),
      compileFilesInDir("tests/pos-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/pos-special/sourcepath/outer", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFile("tests/pos-special/sourcepath/outer/nested/Test4.scala", defaultOptions.and("-sourcepath", "tests/pos-special/sourcepath")),
      compileFilesInDir("tests/pos-scala2", defaultOptions.and("-source", "3.0-migration")),
      compileFilesInDir("tests/pos-custom-args/captures", defaultOptions.and("-language:experimental.captureChecking", "-language:experimental.separationChecking")),
      compileFile("tests/pos-special/utf8encoded.scala", defaultOptions.and("-encoding", "UTF8")),
      compileFile("tests/pos-special/utf16encoded.scala", defaultOptions.and("-encoding", "UTF16")),
      compileDir("tests/pos-special/i18589", defaultOptions.and("-Wsafe-init").without("-Ycheck:all")),
      compileDir("tests/pos-special/i24547", defaultOptions.without("-Ycheck:all")),
      // Run tests for legacy lazy vals
      compileFilesInDir("tests/pos", defaultOptions.and("-Wsafe-init", "-Ylegacy-lazy-vals", "-Ycheck-constraint-deps"), FileFilter.include(TestSources.posLazyValsAllowlist)),
      compileDir("tests/pos-special/java-param-names", defaultOptions.withJavacOnlyOptions("-parameters")),
    ) ::: (
      // TODO create a folder for capture checking tests with the stdlib, or use tests/pos-custom-args/captures under this mode?
      if Properties.usingScalaLibraryCCTasty then List(compileDir("tests/pos-special/stdlib", allowDeepSubtypes))
      else Nil
    )
    val compilationTest = withCoverage(aggregateTests(tests*))
    runWithCoverageOrFallback[PosTestWithCoverage](compilationTest, "Pos")
  }

  @Test def rewrites: Unit = {
    implicit val testGroup: TestGroup = TestGroup("rewrites")

    withCoverage(aggregateTests(
      compileFile("tests/rewrites/rewrites.scala", defaultOptions.and("-source", "3.0-migration").and("-rewrite", "-indent")),
      compileFile("tests/rewrites/rewrites3x.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/rewrites3x-fatal-warnings.scala", defaultOptions.and("-rewrite", "-source", "future-migration", "-Werror")),
      compileFile("tests/rewrites/i21394.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/uninitialized-var.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/with-type-operator.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/private-this.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/alphanumeric-infix-operator.scala", defaultOptions.and("-rewrite", "-source", "future-migration")),
      compileFile("tests/rewrites/filtering-fors.scala", defaultOptions.and("-rewrite", "-source", "3.2-migration")),
      compileFile("tests/rewrites/refutable-pattern-bindings-old.scala", defaultOptions.and("-rewrite", "-source", "3.2-migration")),
      compileFile("tests/rewrites/refutable-pattern-bindings.scala", defaultOptions.and("-rewrite", "-source", "3.8-migration")),
      compileFile("tests/rewrites/i8982.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i9632.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i11895.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/i12340.scala", unindentOptions.and("-rewrite")),
      compileFile("tests/rewrites/i17187.scala", unindentOptions.and("-rewrite")),
      compileFile("tests/rewrites/i17399.scala", unindentOptions.and("-rewrite")),
      compileFile("tests/rewrites/i20002.scala", defaultOptions.and("-indent", "-rewrite")),
      compileDir("tests/rewrites/annotation-named-pararamters", defaultOptions.and("-rewrite", "-source:3.6-migration")),
      compileFile("tests/rewrites/i21418.scala", unindentOptions.and("-rewrite", "-source:3.5-migration")),
      compileFile("tests/rewrites/infix-named-args.scala", defaultOptions.and("-rewrite", "-source:3.7-migration")),
      compileFile("tests/rewrites/ambiguous-named-tuple-assignment.scala", defaultOptions.and("-rewrite", "-source:3.6-migration")),
      compileFile("tests/rewrites/i21382.scala", defaultOptions.and("-indent", "-rewrite")),
      compileFile("tests/rewrites/unused.scala", defaultOptions.and("-rewrite", "-Wunused:all")),
      compileFile("tests/rewrites/i22440.scala", defaultOptions.and("-rewrite")),
      compileFile("tests/rewrites/i22731.scala", defaultOptions.and("-rewrite", "-source:3.7-migration")),
      compileFile("tests/rewrites/i22731b.scala", defaultOptions.and("-rewrite", "-source:3.7-migration")),
      compileFile("tests/rewrites/implicit-to-given.scala", defaultOptions.and("-rewrite", "-Yimplicit-to-given")),
      compileFile("tests/rewrites/i22792.scala", defaultOptions.and("-rewrite")),
      compileFile("tests/rewrites/i23449.scala", defaultOptions.and("-rewrite", "-source:3.4-migration")),
      compileFile("tests/rewrites/i24103.scala", defaultOptions.and("-rewrite", "-source:3.4-migration")),
      compileFile("tests/rewrites/i24103b.scala", defaultOptions.and("-rewrite", "-source:3.4-migration")),
      compileFile("tests/rewrites/i24213.scala", defaultOptions.and("-rewrite", "-source:3.4-migration")),
      compileFile("tests/rewrites/i18234.scala", defaultOptions.and("-rewrite", "-source:3.8-migration")),
    )).checkRewrites()
  }

  @Test def posTwice: Unit = {
    implicit val testGroup: TestGroup = TestGroup("posTwice")
    aggregateTests(
      compileFilesInDir("tests/pos-java-interop", defaultOptions),
      compileFilesInDir("tests/pos-java-interop-separate", defaultOptions),
      compileFile("tests/pos/t2168.scala", defaultOptions),
      compileFile("tests/pos/test-erasure.scala", defaultOptions),
      compileFile("tests/pos/Coder.scala", defaultOptions),
      compileFile("tests/pos/blockescapes.scala", defaultOptions),
      compileFile("tests/pos/functions1.scala", defaultOptions),
      compileFile("tests/pos/test-implicits1.scala", defaultOptions),
      compileFile("tests/pos/inferred.scala", defaultOptions),
      compileFile("tests/pos/selftypes.scala", defaultOptions),
      compileFile("tests/pos/varargs.scala", defaultOptions),
      compileFile("tests/pos/vararg-pattern.scala", defaultOptions),
      compileFile("tests/pos/opassign.scala", defaultOptions),
      compileFile("tests/pos/typedapply.scala", defaultOptions),
      compileFile("tests/pos/nameddefaults.scala", defaultOptions),
      compileFile("tests/pos/test-desugar.scala", defaultOptions),
      compileFile("tests/pos/sigs.scala", defaultOptions),
      compileFile("tests/pos/test-typers.scala", defaultOptions),
      compileDir("tests/pos/typedIdents", defaultOptions),
      compileFile("tests/pos/assignments.scala", defaultOptions),
      compileFile("tests/pos/packageobject.scala", defaultOptions),
      compileFile("tests/pos/overloaded.scala", defaultOptions),
      compileFile("tests/pos/overrides.scala", defaultOptions),
      compileDir("tests/pos/java-override", defaultOptions),
      compileFile("tests/pos/templateParents.scala", defaultOptions),
      compileFile("tests/pos/overloadedAccess.scala", defaultOptions),
      compileFile("tests/pos/approximateUnion.scala", defaultOptions),
      compileFilesInDir("tests/pos/tailcall", defaultOptions),
      compileShallowFilesInDir("tests/pos/pos_valueclasses", defaultOptions),
      compileFile("tests/pos/subtyping.scala", defaultOptions),
      compileFile("tests/pos/i0239.scala", defaultOptions),
      compileFile("tests/pos/anonClassSubtyping.scala", defaultOptions),
      compileFile("tests/pos/extmethods.scala", defaultOptions),
      compileFile("tests/pos/companions.scala", defaultOptions),
      compileFile("tests/pos/main.scala", defaultOptions)
    ).times(2).checkCompile()
  }

  // Warning tests ------------------------------------------------------------

  @Test def warn: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileWarn")
    val compilationTest = withCoverage(aggregateTests(
      compileFilesInDir("tests/warn", defaultOptions),
    ))
    runWithCoverageOrFallback[WarnTestWithCoverage](compilationTest, "Warn")
  }

  // Negative tests ------------------------------------------------------------

  @Test def negAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileNeg")
    aggregateTests(
      compileFilesInDir("tests/neg", defaultOptions, FileFilter.exclude(TestSources.negScala2LibraryTastyExcludelisted)),
      compileFilesInDir("tests/neg-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/neg-custom-args/captures", defaultOptions.and("-language:experimental.captureChecking", "-language:experimental.separationChecking", "-source", "3.8")),
      compileFile("tests/neg-custom-args/sourcepath/outer/nested/Test1.scala", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath")),
      compileDir("tests/neg-custom-args/sourcepath2/hi", defaultOptions.and("-sourcepath", "tests/neg-custom-args/sourcepath2", "-Werror")),
      compileList("duplicate source", List(
        "tests/neg-custom-args/toplevel-samesource/S.scala",
        "tests/neg-custom-args/toplevel-samesource/nested/S.scala"),
        defaultOptions),
      compileFile("tests/neg/i7575.scala", defaultOptions.withoutLanguageFeatures),
      compileFile("tests/neg-custom-args/i20491/Test.scala", defaultOptions.withClasspath("tests/neg-custom-args/i20491/cp")),
    ).checkExpectedErrors()
  }

  @Test def fuzzyAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("compileFuzzy")
    compileFilesInDir("tests/fuzzy", defaultOptions).checkNoCrash()
  }

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runAll")
    val compilationTest = withCoverage(aggregateTests(
      compileFilesInDir("tests/run", defaultOptions.and("-Wsafe-init")),
      compileFilesInDir("tests/run-deep-subtype", allowDeepSubtypes),
      compileFilesInDir("tests/run-custom-args/captures", allowDeepSubtypes.and("-language:experimental.captureChecking", "-language:experimental.separationChecking", "-source", "3.8")),
      // Run tests for legacy lazy vals.
      compileFilesInDir("tests/run", defaultOptions.and("-Wsafe-init", "-Ylegacy-lazy-vals", "-Ycheck-constraint-deps"), FileFilter.include(TestSources.runLazyValsAllowlist)),
    ))
    runWithCoverageOrFallback[RunTestWithCoverage](compilationTest, "Run")
  }

  // Generic java signatures tests ---------------------------------------------

  @Test def genericJavaSignatures: Unit = {
    implicit val testGroup: TestGroup = TestGroup("genericJavaSignatures")
    val compilationTest = withCoverage(compileFilesInDir("tests/generic-java-signatures", defaultOptions))
    runWithCoverageOrFallback[RunTestWithCoverage](compilationTest, "Run")
  }

  // Pickling Tests ------------------------------------------------------------

  @Test def pickling: Unit = {
    implicit val testGroup: TestGroup = TestGroup("testPickling")
    aggregateTests(
      compileFilesInDir("tests/pos", picklingOptions, FileFilter.exclude(TestSources.posTestPicklingExcludelisted)),
      compileFilesInDir("tests/run", picklingOptions, FileFilter.exclude(TestSources.runTestPicklingExcludelisted))
    ).checkCompile()
  }

  //@Test disabled in favor of posWithCompilerCC to save time.
  def recheck: Unit =
    given TestGroup = TestGroup("recheck")
    aggregateTests(
      compileFilesInDir("tests/run", defaultOptions.and("-Yrecheck-test"), FileFilter.exclude(TestSources.runTestRecheckExcluded))
      //Disabled to save some time.
      //compileFilesInDir("tests/pos", recheckOptions, FileFilter.exclude(TestSources.posTestRecheckExcluded)),
    ).checkCompile()

  // Explicit nulls tests
  @Test def explicitNullsNeg: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsNeg")
    aggregateTests(
      compileFilesInDir("tests/explicit-nulls/neg", explicitNullsOptions, FileFilter.exclude(TestSources.negExplicitNullsScala2LibraryTastyExcludelisted)),
      compileFilesInDir("tests/explicit-nulls/flexible-types-common", explicitNullsOptions `and` "-Yno-flexible-types"),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", explicitNullsOptions `and` "-Yno-flexible-types", FileFilter.exclude(TestSources.negExplicitNullsScala2LibraryTastyExcludelisted)),
    ).checkExpectedErrors()

    // locally {
    //   val unsafeFile = compileFile("tests/explicit-nulls/flexible-unpickle/neg/Unsafe_1.scala", explicitNullsOptions without "-Yexplicit-nulls")
    //   val flexibleFile = compileFile("tests/explicit-nulls/flexible-unpickle/neg/Flexible_2.scala",
    //       explicitNullsOptions.and("-Yflexify-tasty").withClasspath(defaultOutputDir + testGroup + "/Unsafe_1/neg/Unsafe_1"))

    //   unsafeFile.keepOutput.checkCompile()
    //   flexibleFile.keepOutput.checkExpectedErrors()

    //   List(unsafeFile, flexibleFile).foreach(_.delete())
    // }
  }

  @Test def explicitNullsPos: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsPos")
    val compilationTest = withCoverage(aggregateTests(
      compileFilesInDir("tests/explicit-nulls/pos", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/flexible-types-common", explicitNullsOptions),
      compileFilesInDir("tests/explicit-nulls/unsafe-common", explicitNullsOptions `and` "-language:unsafeNulls" `and` "-Yno-flexible-types"),
    ))
    runWithCoverageOrFallback[PosTestWithCoverage](compilationTest, "Pos")

    // The regression test for i25722 has some atypical classpath requirements.
    // The test consists of (a) one Java nullability annotation, (b) one Java user of the annotation, and (c) two Scala files,
    // which must be compiled separately. In addition:
    //   - the output from (a) must be on the classpath while compiling (b)
    //   - the output from (b) must be on the classpath while compiling (c)
    //   - the output from (a) _must not_ be on the classpath while compiling (c)
    locally {
      val i25722Group = TestGroup("tests/explicit-nulls/special/i25722")
      val i25722Options = explicitNullsOptions.and("-Yforce-sbt-phases")
      val outDir1 = Paths.get(defaultOutputDir.getAbsolutePath, i25722Group.name, "Nullable", "annotations", "Nullable/").toString
      val outDir2 = Paths.get(defaultOutputDir.getAbsolutePath, i25722Group.name, "Foo", "lib", "Foo").toString
      val tests = List(
        withCoverage(compileFile("tests/explicit-nulls/special/25722/jstubs/jstubs/org/jetbrains/annotations/Nullable.java", i25722Options)(using i25722Group).keepOutput),
        withCoverage(compileFile("tests/explicit-nulls/special/25722/jstubs/jstubs/lib/Foo.java", i25722Options.withClasspath(outDir1))(using i25722Group).keepOutput),
        withCoverage(compileDir("tests/explicit-nulls/special/25722/scala", i25722Options.withClasspath(outDir2))(using i25722Group).keepOutput)
      )
      tests.foreach(t => runWithCoverageOrFallback[PosTestWithCoverage](t, "Pos"))
      tests.foreach(_.delete())
    }

    // locally {
    //   val tests = List(
    //     compileFile("tests/explicit-nulls/flexible-unpickle/pos/Unsafe_1.scala", explicitNullsOptions without "-Yexplicit-nulls"),
    //     compileFile("tests/explicit-nulls/flexible-unpickle/pos/Flexible_2.scala",
    //     explicitNullsOptions.and("-Yflexify-tasty").withClasspath(defaultOutputDir + testGroup + "/Unsafe_1/pos/Unsafe_1")),
    //   ).map(_.keepOutput.checkCompile())

    //   tests.foreach(_.delete())
    // }
  }

  @Test def explicitNullsWarn: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsWarn")
    val compilationTest = withCoverage(compileFilesInDir("tests/explicit-nulls/warn", explicitNullsOptions))
    runWithCoverageOrFallback[WarnTestWithCoverage](compilationTest, "Warn")
  }

  @Test def explicitNullsRun: Unit = {
    implicit val testGroup: TestGroup = TestGroup("explicitNullsRun")
    val compilationTest = withCoverage(compileFilesInDir("tests/explicit-nulls/run", explicitNullsOptions))
    runWithCoverageOrFallback[RunTestWithCoverage](compilationTest, "Run")
  }

  // initialization tests for global objects
  // Scoverage coverage disabled: majority of tests fail (coverage instrumentation triggers extra -Ysafe-init-global warnings)
  @Test def checkInitGlobal: Unit = {
    implicit val testGroup: TestGroup = TestGroup("checkInitGlobal")
    compileFilesInDir("tests/init-global/warn", defaultOptions.and("-Ysafe-init-global"), FileFilter.exclude(TestSources.negInitGlobalScala2LibraryTastyExcludelisted)).checkWarnings()
    compileFilesInDir("tests/init-global/pos", defaultOptions.and("-Ysafe-init-global", "-Werror"), FileFilter.exclude(TestSources.posInitGlobalScala2LibraryTastyExcludelisted)).checkCompile()
    if Properties.usingScalaLibraryTasty && !Properties.usingScalaLibraryCCTasty then
      compileFilesInDir("tests/init-global/warn-tasty", defaultOptions.and("-Ysafe-init-global"), FileFilter.exclude(TestSources.negInitGlobalScala2LibraryTastyExcludelisted)).checkWarnings()
      compileFilesInDir("tests/init-global/pos-tasty", defaultOptions.and("-Ysafe-init-global", "-Werror"), FileFilter.exclude(TestSources.posInitGlobalScala2LibraryTastyExcludelisted)).checkCompile()
    end if

    locally {
      val group = TestGroup("checkInitGlobal/tastySource")
      val tastSourceOptions = defaultOptions.and("-Ysafe-init-global")
      val outDirLib = Paths.get(defaultOutputDir.getAbsolutePath, group.name,"A", "tastySource", "A").toString

      // Set -sourceroot such that the source code cannot be found by the compiler
      val libOptions = tastSourceOptions.and("-sourceroot", "tests/init-global/special")
      val lib = compileFile("tests/init-global/special/tastySource/A.scala", libOptions)
        (using group)
        .keepOutput
        .checkCompile()

      compileFile("tests/init-global/special/tastySource/B.scala", tastSourceOptions.withClasspath(outDirLib))
        (using group)
        .checkWarnings()

      lib.delete()
    }
  }

  // initialization tests
  @Test def safeInit: Unit = {
    given TestGroup = TestGroup("safeInit")
    val options = defaultOptions.and("-Wsafe-init", "-Werror")
    compileFilesInDir("tests/init/neg", options).checkExpectedErrors()
    val initWarnTest = withCoverage(compileFilesInDir("tests/init/warn", defaultOptions.and("-Wsafe-init")))
    runWithCoverageOrFallback[WarnTestWithCoverage](initWarnTest, "Warn")
    val initPosTest = withCoverage(compileFilesInDir("tests/init/pos", options))
    runWithCoverageOrFallback[PosTestWithCoverage](initPosTest, "Pos")
    val initCrashTest = withCoverage(compileFilesInDir("tests/init/crash", options.without("-Werror")))
    runWithCoverageOrFallback[PosTestWithCoverage](initCrashTest, "Pos")
    // The regression test for i12128 has some atypical classpath requirements.
    // The test consists of three files: (a) Reflect_1  (b) Macro_2  (c) Test_3
    // which must be compiled separately. In addition:
    //   - the output from (a) must be on the classpath while compiling (b)
    //   - the output from (b) must be on the classpath while compiling (c)
    //   - the output from (a) _must not_ be on the classpath while compiling (c)
    locally {
      val i12128Group = TestGroup("checkInit/i12128")
      val i12128Options = options.without("-Werror")
      val outDir1 = Paths.get(defaultOutputDir.getAbsolutePath, i12128Group.name, "Reflect_1", "i12128", "Reflect_1").toString
      val outDir2 = Paths.get(defaultOutputDir.getAbsolutePath, i12128Group.name, "Macro_2", "i12128", "Macro_2").toString

      val tests = List(
        withCoverage(compileFile("tests/init/special/i12128/Reflect_1.scala", i12128Options)(using i12128Group).keepOutput),
        withCoverage(compileFile("tests/init/special/i12128/Macro_2.scala", i12128Options.withClasspath(outDir1))(using i12128Group).keepOutput),
        withCoverage(compileFile("tests/init/special/i12128/Test_3.scala", options.withClasspath(outDir2))(using i12128Group).keepOutput)
      )
      tests.foreach(t => runWithCoverageOrFallback[PosTestWithCoverage](t, "Pos"))
      tests.foreach(_.delete())
    }

    /* This tests for errors in the program's TASTy trees.
     * The test consists of three files: (a) v1/A, (b) v1/B, and (c) v0/A. (a) and (b) are
     * compatible, but (b) and (c) are not. If (b) and (c) are compiled together, there should be
     * an error when reading the files' TASTy trees. */
    locally {
      val tastyErrorGroup = TestGroup("checkInit/tasty-error/val-or-defdef")
      val tastyErrorOptions = options.without("-Werror")

      val classA0 = Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "A", "v0", "A").toString
      val classA1 = Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "A", "v1", "A").toString
      val classB1 = Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "B", "v1", "B").toString

      val tests = List(
        withCoverage(compileFile("tests/init/tasty-error/val-or-defdef/v1/A.scala", tastyErrorOptions)(using tastyErrorGroup).keepOutput),
        withCoverage(compileFile("tests/init/tasty-error/val-or-defdef/v1/B.scala", tastyErrorOptions.withClasspath(classA1))(using tastyErrorGroup).keepOutput),
        withCoverage(compileFile("tests/init/tasty-error/val-or-defdef/v0/A.scala", tastyErrorOptions)(using tastyErrorGroup).keepOutput),
      )
      tests.foreach(t => runWithCoverageOrFallback[PosTestWithCoverage](t, "Pos"))

      compileFile("tests/init/tasty-error/val-or-defdef/Main.scala", tastyErrorOptions.withClasspath(classA0).withClasspath(classB1))(using tastyErrorGroup).checkExpectedErrors()

      tests.foreach(_.delete())
    }

    /* This tests for errors in the program's TASTy trees.
     * The test consists of five files: Main, C, v1/A, v1/B, and v0/A. The files v1/A, v1/B, and v0/A all depend on C. v1/A and v1/B are
     * compatible, but v1/B and v0/A are not. If v1/B and v0/A are compiled together, there should be
     * an error when reading the files' TASTy trees. This fact is demonstrated by the compilation of Main. */
    locally {
      val tastyErrorGroup = TestGroup("checkInit/tasty-error/typedef")
      val tastyErrorOptions = options.without("-Werror").without("-Ycheck:all")

      val classC =  Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "C", "typedef", "C").toString
      val classA0 = Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "A", "v0", "A").toString
      val classA1 = Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "A", "v1", "A").toString
      val classB1 = Paths.get(defaultOutputDir.getAbsolutePath, tastyErrorGroup.name, "B", "v1", "B").toString

      val tests = List(
        withCoverage(compileFile("tests/init/tasty-error/typedef/C.scala", tastyErrorOptions)(using tastyErrorGroup).keepOutput),
        withCoverage(compileFile("tests/init/tasty-error/typedef/v1/A.scala", tastyErrorOptions.withClasspath(classC))(using tastyErrorGroup).keepOutput),
        withCoverage(compileFile("tests/init/tasty-error/typedef/v1/B.scala", tastyErrorOptions.withClasspath(classC).withClasspath(classA1))(using tastyErrorGroup).keepOutput),
        withCoverage(compileFile("tests/init/tasty-error/typedef/v0/A.scala", tastyErrorOptions.withClasspath(classC))(using tastyErrorGroup).keepOutput),
      )
      tests.foreach(t => runWithCoverageOrFallback[PosTestWithCoverage](t, "Pos"))

      compileFile("tests/init/tasty-error/typedef/Main.scala", tastyErrorOptions.withClasspath(classC).withClasspath(classA0).withClasspath(classB1))(using tastyErrorGroup).checkExpectedErrors()

      tests.foreach(_.delete())
    }
  }

  // parallel backend tests
  @Test def parallelBackend: Unit = {
    given TestGroup = TestGroup("parallelBackend")
    val parallelism = Runtime.getRuntime().availableProcessors().min(16)
    assumeTrue("Not enough available processors to run parallel tests", parallelism > 1)

    val options = defaultOptions.and(s"-Ybackend-parallelism:${parallelism}")
    def parCompileDir(directory: String) = compileDir(directory, options)

    // Compilation units containing more than 1 source file
    aggregateTests(
      parCompileDir("tests/pos/i10477"),
      parCompileDir("tests/pos/i4758"),
      parCompileDir("tests/pos/scala2traits"),
      parCompileDir("tests/pos/class-gadt"),
      parCompileDir("tests/pos/tailcall"),
      parCompileDir("tests/pos/reference"),
      parCompileDir("tests/pos/pos_valueclasses")
    ).checkCompile()

    aggregateTests(
      parCompileDir("tests/neg/package-implicit"),
      parCompileDir("tests/neg/package-export")
    ).checkExpectedErrors()

    aggregateTests(
      parCompileDir("tests/run/decorators"),
      parCompileDir("tests/run/generic")
    ).checkRuns()

  }
}

object CompilationTests extends ParallelTesting with CoverageSupport {
  // Test suite configuration --------------------------------------------------

  def maxDuration = 45.seconds
  def numberOfWorkers = Runtime.getRuntime().availableProcessors()
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  given summaryReport: SummaryReporting = new SummaryReport

  @AfterClass def tearDown(): Unit = {
    super.cleanup()
    summaryReport.echoSummary()
  }

}
