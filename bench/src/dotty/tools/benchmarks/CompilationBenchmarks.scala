package dotty.tools.benchmarks

import java.util.concurrent.TimeUnit.MILLISECONDS
import java.io.File

import scala.sys.process.{ProcessBuilder, stringToProcess}

import org.openjdk.jmh.annotations.{Fork, Warmup, Measurement, BenchmarkMode, Benchmark, State, Setup, Scope, Level, OutputTimeUnit}

import dotty.tools.dotc.{Driver, Run, Compiler}
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Types.{TermRef, Type}
import dotty.tools.dotc.core.Contexts.{ContextBase, Context, ctx, withMode}

/** Compilation benchmarks.
  *
  * Results over time can be visualized at https://dotty-bench.epfl.ch.
  *
  * Pre-requisites: the benchmarks initialization runs the `cs` (coursier), `rm`
  * and `mkdir` commands. It is only expected to work on Unix-like systems.
  *
  * The `CompilationBenchmarks` class below is processed and run by JMH.
  *
  * To run all compilation benchmarks, run the following command from SBT:
  * ```
  * scala3-bench / Jmh / run CompilationBenchmarks
  * ```
  *
  * Each benchmark is a method annotated with `@Benchmark`.
  *
  * Benchmarks that should be run nightly contains `Nightly` in their name.
  * Benchmarks that should be run using the bootstrapped compiler contains
  * `Bootstrapped` in their name.
  *
  * To run only benchmarks that should _not_ be run nightly nor using the
  * bootstrapped compiler, you can use the following command (`-e` is for
  * excluding):
  *
  * ```
  * scala3-bench / Jmh / run -e Nightly -e Bootstrapped
  * ```
  *
  * To run only benchmarks that should be run nightly, you can use the following
  * command:
  * ```
  * scala3-bench / Jmh / run Nightly
  * ```
  *
  * You can also filter by other parts of the (fully-qualified) benchmark method
  * name. For example, to only run the `dottyNightly` benchmark, you can use:
  * ```
  * scala3-bench / Jmh / run CompilationBenchmarks.dotty
  * ```
  *
  * To test benchmarks quickly, you can override the number of warmup and
  * measurement iterations respectively using the `-wi` and `-i` options:
  * ```
  * scala3-bench / Jmh / run -wi 1 -i 2 CompilationBenchmarks.implicitNumsTasty
  * ```
  *
  * Use `-help` to list all JMH options:
  * ```
  * scala3-bench / Jmh / run -help
  * ```
  *
  * The benchmarks that use files from `tests/bench` are also run as part of
  * normal tests. You can run these with:
  * ```
  * scala3-compiler-bootstrapped / testOnly dotty.tools.dotc.BootstrappedOnlyCompilationTests -- *posBenchs*
  * ```
  *
  * Important: the `@Benchmark` methods, must be kept minimal to avoid any side
  * effects that could affect the benchmark results.
  *
  * Useful references:
  *   - JMH examples:
  *     https://github.com/openjdk/jmh/tree/master/jmh-samples/src/main/java/org/openjdk/jmh/samples
  *   - JMH annotations documentation:
  *     https://javadoc.io/doc/org.openjdk.jmh/jmh-core/1.1.1/org/openjdk/jmh/annotations/package-summary.html
  */
@Fork(value = 1, jvmArgsAppend = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 120) // default, overriden below for some benchmarks
@Measurement(iterations = 30) // default, overriden below for some benchmarks
@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.SingleShotTime))
@State(Scope.Benchmark)
@OutputTimeUnit(MILLISECONDS)
class CompilationBenchmarks:
  /** Temporary output directory for compilation. Deleted between iterations. */
  val tmp = "tmp"

  /** Output directory for files to be kept. This is used by benchmarks that
    * compile from TASTy, such as `implicitCacheTasty`.
    */
  val out = "out"

  /** Directory in which to generate synthetic benchmarks. */
  val generated = "tests-generated"

  /** Launches `scalac` with the given arguments. */
  def compile(args: Array[String], out: String = tmp) =
    // Run benchmark with the default benchmarks classpath if not specified.
    val classPathArg = if args.contains("-classpath") then Array.empty[String] else Array("-classpath", benchClassPath)
    val allArgs = Array("-d", out) ++ classPathArg ++ args
    Driver().process(allArgs)

  /** Finds files in `d` and its subdirectories that satisfy `p`. */
  def find(d: String, p: String => Boolean): Array[String] =
    def rec(d: File, p: String => Boolean): Array[String] =
      val children = d.listFiles
      children.map(_.toString).filter(p) ++ children.filter(_.isDirectory).flatMap(rec(_, p))
    val f = File(d)
    if !f.exists then throw new IllegalArgumentException(s"Directory ${f.getAbsolutePath()} does not exist")
    rec(f, p)

  /** Finds files with the `scala` extension in `d` and its subdirectories. */
  def findScalaFiles(d: String) = find(d, _.endsWith(".scala"))

  /** Get the value of a system property, fail if it is not set. */
  def getDefinedProperties(name: String) =
    val res = System.getProperty(name)
    assert(res != null, s"$name must be set")
    res

  /** Class path of `scala3-library-bootstrapped`. Defined in `Build.scala`. */
  val benchClassPath = getDefinedProperties("BENCH_CLASS_PATH")

  /** Class path of `scala3-bootstrapped`. Defined in `Build.scala`. */
  val benchCompilerClassPath = getDefinedProperties("BENCH_COMPILER_CLASS_PATH")

  @Setup(Level.Trial)
  def trialSetup(): Unit =
    // Clean up output directory. Using `rm` instead of Java's API because it
    // seems better to remove the whole directory atomically. Got occasional
    // `DirectoryNotEmptyException` exceptions with the Java's API.
    s"rm -rf $out".!
    s"mkdir -p $out".!

    // Compile `implicit_cache.scala` and `implicitNums.scala` to TASTy, for use
    // by the `implicitCacheTasty` and `implicitNumsTasty` benchmarks.
    compile(implicitCacheArgs, out)
    compile(implicitNumsArgs, out)

    // Generates benchmarks in `tests-generated`. These are big (>500 kB) source
    // files that are probably better generated on the fly than stored in the
    // repository.
    generateBenchmarks(generated)

  @Setup(Level.Iteration)
  def setup(): Unit =
    s"rm -rf $tmp".!
    s"mkdir -p $tmp".!

  val dottyArgs =
    val sources = find("../compiler/src/dotty", f => f.endsWith(".scala") || f.endsWith(".java"))
    // format: off
    Array(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Xfatal-warnings",
      "-encoding", "UTF8",
      "-language:implicitConversions",
      "-Yexplicit-nulls",
      "-Wsafe-init",
      "-source", "3.3",
      "-classpath", benchCompilerClassPath
    ) ++ sources
    // format: on

  @Warmup(iterations = 8)
  @Measurement(iterations = 8)
  @Benchmark
  def dottyNightly() = compile(dottyArgs)

  val dottySbtArgs = Array("-Yforce-sbt-phases") ++ dottyArgs

  @Warmup(iterations = 8)
  @Measurement(iterations = 8)
  @Benchmark
  def dottySbtNightly() = compile(dottySbtArgs)

  val stdlibArgs =
    val sources = find(
      "../community-build/community-projects/stdLib213/src/library/scala",
      f => (f.endsWith(".scala") || f.endsWith(".java")) && !f.endsWith("language.scala") && !f.endsWith("AnyVal.scala")
    )
    Array("-language:implicitConversions", "-Wconf:any:s", "-source", "3.3") ++ sources

  @Warmup(iterations = 24)
  @Measurement(iterations = 16)
  @Benchmark
  def stdlibNightly() = compile(stdlibArgs)

  val scalapArgs =
    val sources = findScalaFiles("../community-build/community-projects/scalap/src/scalap")
    val depClasspath = "cs fetch -p org.scala-lang:scala-compiler:2.13.0".!!
    Array("-source", "3.0-migration", "-Wconf:any:s", "-classpath", s"$depClasspath:$benchClassPath") ++ sources

  @Benchmark def scalap() = compile(scalapArgs)

  val re2sArgs = Array("-Wconf:any:s") ++ findScalaFiles("../tests/bench/re2s/src")
  @Benchmark def re2s() = compile(re2sArgs)

  val implicitCacheArgs = Array("../tests/bench/implicit_cache.scala")
  @Benchmark def implicitCacheBootstrapped() = compile(implicitCacheArgs)

  val implicitCacheTastyArgs = Array("-from-tasty", s"$out/Test.tasty", s"$out/A.tasty", s"$out/Foo.tasty")
  @Benchmark def implicitCacheTasty() = compile(implicitCacheTastyArgs)

  val implicitNumsArgs = Array("../tests/bench/implicitNums.scala")
  @Benchmark def implicitNumsBootstrapped() = compile(implicitNumsArgs)

  val implicitNumsTastyArgs = Array("-from-tasty", s"$out/Test.tasty")
  @Benchmark def implicitNumsTasty() = compile(implicitNumsTastyArgs)

  val implicitScopeLoopArgs = Array("../tests/bench/implicit-scope-loop.scala")
  @Benchmark def implicitScopeLoop() = compile(implicitScopeLoopArgs)

  // Crashes since 2024-04.
  // From https://github.com/scala/scala3/pull/20195 ?
  // TODO: fix and re-enable
  // @Benchmark
  val inductiveImplicitsArgs = Array("../tests/bench/inductive-implicits.scala")
  def inductiveImplicits() = compile(inductiveImplicitsArgs)

  val findRefArgs = Array("../tests/bench/FindRef.scala")
  @Benchmark def findRef() = compile(findRefArgs)
