package dotty
package tools
package dotc

import org.junit.Test
import java.io.{ File => JFile }

import org.junit.experimental.categories.Category

@Category(Array(classOf[ParallelTesting]))
class CompilationTests extends ParallelTesting {
  import CompilationTests._

  def interactive: Boolean = !sys.env.contains("DRONE")

  def regex: Option[String] = sys.props.get("dotty.partest.filter")

  // Positive tests ------------------------------------------------------------

  @Test def compilePos: Unit = {
    compileList("compileStdLib", StdLibSources.whitelisted, scala2Mode.and("-migration", "-Yno-inline")) +
    compileFilesInDir("../tests/pos", defaultOptions)
  }.pos()

  @Test def compilePosScala2: Unit =
    compileFilesInDir("../tests/pos-scala2", scala2Mode).pos()

  @Test def compilePosMixedFlags: Unit = {
    compileFile("../tests/pos/nullarify.scala", defaultOptions.and("-Ycheck:nullarify")) +
    compileFile("../tests/pos-scala2/rewrites.scala", scala2Mode.and("-rewrite")).copyToTarget() +
    compileFile("../tests/pos-special/t8146a.scala", allowDeepSubtypes) +
    compileFile("../tests/pos-special/utf8encoded.scala", explicitUTF8) +
    compileFile("../tests/pos-special/utf16encoded.scala", explicitUTF16) +
    compileList(
      "compileMixed",
      List(
        "../tests/pos/B.scala",
        "../scala-scala/src/library/scala/collection/immutable/Seq.scala",
        "../scala-scala/src/library/scala/collection/parallel/ParSeq.scala",
        "../scala-scala/src/library/scala/package.scala",
        "../scala-scala/src/library/scala/collection/GenSeqLike.scala",
        "../scala-scala/src/library/scala/collection/SeqLike.scala",
        "../scala-scala/src/library/scala/collection/generic/GenSeqFactory.scala"
      ),
      defaultOptions
    ) +
    compileFilesInDir("../tests/pos-special/spec-t5545", defaultOptions) +
    compileFile("../scala-scala/src/library/scala/collection/immutable/IndexedSeq.scala", defaultOptions) +
    compileFile("../scala-scala/src/library/scala/collection/parallel/mutable/ParSetLike.scala", defaultOptions) +
    compileList(
      "parSetSubset",
      List(
       "../scala-scala/src/library/scala/collection/parallel/mutable/ParSetLike.scala",
       "../scala-scala/src/library/scala/collection/parallel/mutable/ParSet.scala",
       "../scala-scala/src/library/scala/collection/mutable/SetLike.scala"
      ),
      scala2Mode
    )
  }.pos()

  @Test def compileCoreNoCheck: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core", noCheckOptions ++ classPath).pos()

  @Test def compileDotcInternals: Unit = {
    compileDir("../compiler/src/dotty/tools/dotc/ast", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/config", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core", allowDeepSubtypes) +
    compileDir("../compiler/src/dotty/tools/dotc/transform", allowDeepSubtypes) +
    compileDir("../compiler/src/dotty/tools/dotc/parsing", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/printing", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/reporting", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/typer", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/util", defaultOptions) +
    compileDir("../compiler/src/dotty/tools/io", defaultOptions)
  }.pos()

  @Test def posTwice: Unit = {
    compileFile("../tests/pos/Labels.scala", defaultOptions) +
    compileFilesInDir("../tests/pos-java-interop", defaultOptions) +
    compileFile("../tests/pos/t2168.scala", defaultOptions) +
    compileFile("../tests/pos/erasure.scala", defaultOptions) +
    compileFile("../tests/pos/Coder.scala", defaultOptions) +
    compileFile("../tests/pos/blockescapes.scala", defaultOptions) +
    compileFile("../tests/pos/collections.scala", defaultOptions) +
    compileFile("../tests/pos/functions1.scala", defaultOptions) +
    compileFile("../tests/pos/implicits1.scala", defaultOptions) +
    compileFile("../tests/pos/inferred.scala", defaultOptions) +
    compileFile("../tests/pos/Patterns.scala", defaultOptions) +
    compileFile("../tests/pos/selftypes.scala", defaultOptions) +
    compileFile("../tests/pos/varargs.scala", defaultOptions) +
    compileFile("../tests/pos/vararg-pattern.scala", defaultOptions) +
    compileFile("../tests/pos/opassign.scala", defaultOptions) +
    compileFile("../tests/pos/typedapply.scala", defaultOptions) +
    compileFile("../tests/pos/nameddefaults.scala", defaultOptions) +
    compileFile("../tests/pos/desugar.scala", defaultOptions) +
    compileFile("../tests/pos/sigs.scala", defaultOptions) +
    compileFile("../tests/pos/typers.scala", defaultOptions) +
    compileDir("../tests/pos/typedIdents", defaultOptions) +
    compileFile("../tests/pos/assignments.scala", defaultOptions) +
    compileFile("../tests/pos/packageobject.scala", defaultOptions) +
    compileFile("../tests/pos/overloaded.scala", defaultOptions) +
    compileFile("../tests/pos/overrides.scala", defaultOptions) +
    compileDir("../tests/pos/java-override", defaultOptions) +
    compileFile("../tests/pos/templateParents.scala", defaultOptions) +
    compileFile("../tests/pos/overloadedAccess.scala", defaultOptions) +
    compileFile("../tests/pos/approximateUnion.scala", defaultOptions) +
    compileFilesInDir("../tests/pos/tailcall", defaultOptions) +
    compileShallowFilesInDir("../tests/pos/pos_valueclasses", defaultOptions) +
    compileFile("../tests/pos/subtyping.scala", defaultOptions) +
    compileFile("../tests/pos/i0239.scala", defaultOptions) +
    compileFile("../tests/pos/anonClassSubtyping.scala", defaultOptions) +
    compileFile("../tests/pos/extmethods.scala", defaultOptions) +
    compileFile("../tests/pos/companions.scala", defaultOptions) +
    compileList(
      "testNonCyclic",
      List(
        "../compiler/src/dotty/tools/dotc/CompilationUnit.scala",
        "../compiler/src/dotty/tools/dotc/core/Types.scala",
        "../compiler/src/dotty/tools/dotc/ast/Trees.scala"
      ),
      defaultOptions.and("-Xprompt")
    ) +
    compileList(
      "testIssue34",
      List(
        "../compiler/src/dotty/tools/dotc/config/Properties.scala",
        "../compiler/src/dotty/tools/dotc/config/PathResolver.scala"
      ),
      defaultOptions.and("-Xprompt")
    )
  }.times(2).pos()

  // New tests -----------------------------------------------------------------

  @Test def compileNew: Unit =
    compileFilesInDir("../tests/new", defaultOptions).pos()

  // Negative tests ------------------------------------------------------------

  @Test def compileNeg: Unit =
    compileShallowFilesInDir("../tests/neg", defaultOptions).neg()

  @Test def compileNegCustomFlags: Unit = {
    compileFile("../tests/neg/customArgs/typers.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/overrideClass.scala", scala2Mode) +
    compileFile("../tests/neg/customArgs/autoTuplingTest.scala", defaultOptions.and("-language:noAutoTupling")) +
    compileFile("../tests/neg/customArgs/i1050.scala", defaultOptions.and("-strict")) +
    compileFile("../tests/neg/customArgs/i1240.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/i2002.scala", allowDoubleBindings) +
    compileFile("../tests/neg/customArgs/nopredef.scala", defaultOptions.and("-Yno-predef")) +
    compileFile("../tests/neg/customArgs/noimports.scala", defaultOptions.and("-Yno-imports")) +
    compileFile("../tests/neg/customArgs/noimports2.scala", defaultOptions.and("-Yno-imports")) +
    compileFile("../tests/neg/tailcall/t1672b.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/t3275.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/t6574.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/tailrec.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/tailrec-2.scala", defaultOptions) +
    compileFile("../tests/neg/tailcall/tailrec-3.scala", defaultOptions) +
    compileDir("../tests/neg/typedIdents", defaultOptions)
  }.neg()

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit =
    compileFilesInDir("../tests/run", defaultOptions).run()

  // Pickling Tests ------------------------------------------------------------

  @Test def testPickling1: Unit = {
    compileFilesInDir("../tests/new", picklingOptions) +
    compileFilesInDir("../tests/pickling", picklingOptions) +
    compileDir("../library/src/dotty/runtime", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/backend/jvm", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/ast", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/config", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/parsing", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/printing", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/repl", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/rewrite", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/transform", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/typer", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/util", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/io", picklingOptions) +
    compileFile("../tests/pos/pickleinf.scala", picklingOptions)
  }.limitThreads(4).pos()

  @Test def testPickling2: Unit = {
    compileDir("../compiler/src/dotty/tools/dotc/core/classfile", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core/tasty", picklingOptions) +
    compileDir("../compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingOptions)
  }.limitThreads(4).pos()

  @Test def testPickling3: Unit = {
    compileDir("../compiler/src/dotty/tools", picklingOptions)
  }.limitThreads(4).pos()

  @Test def testPickling4: Unit = {
    compileDir("../compiler/src/dotty/tools/dotc", picklingOptions)
  }.limitThreads(4).pos()

  /** The purpose of this test is two-fold, being able to compile dotty
   *  bootstrapped, and making sure that TASTY can link against a compiled
   *  version of Dotty
   */
  @Test def tastyBootstrap: Unit = {
    val opt = Array(
      "-classpath",
      // compile with bootstrapped library on cp:
      defaultOutputDir + "lib$1/src/:" +
      // as well as bootstrapped compiler:
      defaultOutputDir + "dotty1$1/dotty/:" +
      Jars.dottyInterfaces
    )

    def lib =
      compileDir("../library/src",
        allowDeepSubtypes.and("-Ycheck-reentrant", "-strict", "-priorityclasspath", defaultOutputDir))

    def dotty1 =
      compileDir("../compiler/src/dotty", opt)

    def dotty2 =
      compileShallowFilesInDir("../compiler/src/dotty", opt)

    {
      lib.keepOutput :: dotty1.keepOutput :: {
        dotty2 +
        compileShallowFilesInDir("../compiler/src/dotty/tools", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/ast", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/config", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/parsing", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/printing", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/repl", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/reporting", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/rewrite", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/transform", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/typer", opt) +
        compileShallowFilesInDir("../compiler/src/dotty/tools/dotc/util", opt)
      } :: Nil
    }.map(_.pos()).foreach(_.delete())
  }
}

object CompilationTests {
  implicit val defaultOutputDir: String = "../out/"

  implicit class RichStringArray(val xs: Array[String]) extends AnyVal {
    def and(args: String*): Array[String] = {
      val argsArr: Array[String] = args.toArray
      xs ++ argsArr
    }
  }

  val noCheckOptions = Array(
    "-pagewidth", "120",
    "-color:never"
  )

  val checkOptions = Array(
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-Yforce-sbt-phases"
  )

  val classPath = {
    val paths = Jars.dottyTestDeps map { p =>
      val file = new JFile(p)
      assert(
        file.exists,
        s"""|File "$p" couldn't be found. Run `packageAll` from build tool before
            |testing.
            |
            |If running without sbt, test paths need to be setup environment variables:
            |
            | - DOTTY_LIBRARY
            | - DOTTY_COMPILER
            | - DOTTY_INTERFACES
            | - DOTTY_EXTRAS
            |
            |Where these all contain locations, except extras which is a colon
            |separated list of jars.
            |
            |When compiling with eclipse, you need the sbt-interfaces jar, put
            |it in extras."""
      )
      file.getAbsolutePath
    } mkString (":")

    Array("-classpath", paths)
  }

  private val yCheckOptions = Array("-Ycheck:tailrec,resolveSuper,mixin,restoreScopes,labelDef")

  val defaultOptions = noCheckOptions ++ checkOptions ++ yCheckOptions ++ classPath
  val allowDeepSubtypes = defaultOptions diff Array("-Yno-deep-subtypes")
  val allowDoubleBindings = defaultOptions diff Array("-Yno-double-bindings")
  val picklingOptions = defaultOptions ++ Array(
    "-Xprint-types",
    "-Ytest-pickler",
    "-Ystop-after:pickler",
    "-Yprintpos"
  )
  val scala2Mode = defaultOptions ++ Array("-language:Scala2")
  val explicitUTF8 = defaultOptions ++ Array("-encoding", "UTF8")
  val explicitUTF16 = defaultOptions ++ Array("-encoding", "UTF16")
}
