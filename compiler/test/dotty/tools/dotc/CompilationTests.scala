package dotty
package tools
package dotc

import org.junit.Test
import java.io.{ File => JFile }

class CompilationTests extends ParallelTesting {
  import CompilationTests._

  // Positive tests ------------------------------------------------------------

  @Test def compilePos: Unit =
    compileFilesInDir("../tests/pos", defaultOptions).pos

  @Test def compilePosScala2: Unit =
    compileFilesInDir("../tests/pos-scala2", scala2Mode).pos

  @Test def nullarifyPos: Unit =
    compileFile("../tests/pos/nullarify.scala", defaultOptions.and("-Ycheck:nullarify")).pos

  @Test def rewrites: Unit =
    compileFile("../tests/pos-scala2/rewrites.scala", scala2Mode.and("-rewrite")).pos

  @Test def t8146aPos: Unit =
    compileFile("../tests/pos-special/t8146a.scala", allowDeepSubtypes).pos

  @Test def t5545Pos: Unit =
    compileFilesInDir("../tests/pos-special/spec-t5545", defaultOptions).pos

  @Test def utf8encodedPos: Unit =
    compileFile("../tests/pos-special/utf8encoded.scala", explicitUTF8).pos

  @Test def utf16encodedPos: Unit =
    compileFile("../tests/pos-special/utf16encoded.scala", explicitUTF16).pos

  @Test def compileStdLibPos: Unit =
    compileList(StdLibSources.whitelisted, scala2Mode.and("-migration", "-Yno-inline")).pos

  @Test def compileMixedPos: Unit = compileList(
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
  ).pos

  @Test def compileIndexedSeqPos: Unit =
    compileFile("../scala-scala/src/library/scala/collection/immutable/IndexedSeq.scala", defaultOptions).pos

  @Test def compileParSetLikePos: Unit =
    compileFile("../scala-scala/src/library/scala/collection/parallel/mutable/ParSetLike.scala", defaultOptions).pos

  @Test def compileParSetSubsetPos: Unit = compileList(
    List(
     "../scala-scala/src/library/scala/collection/parallel/mutable/ParSetLike.scala",
     "../scala-scala/src/library/scala/collection/parallel/mutable/ParSet.scala",
     "../scala-scala/src/library/scala/collection/mutable/SetLike.scala"
    ),
    scala2Mode
  ).pos

  @Test def compileAstPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/ast", defaultOptions).pos

  @Test def compileConfigPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/config", defaultOptions).pos

  @Test def compileCorePos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core", allowDeepSubtypes).pos

  @Test def compileCoreNoCheckPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core", noCheckOptions ++ classPath).pos

  @Test def compileTransformPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/transform", allowDeepSubtypes).pos

  @Test def compileParsingPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/parsing", defaultOptions).pos

  @Test def compilePrintingPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/printing", defaultOptions).pos

  @Test def compileReportingPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/reporting", defaultOptions).pos

  @Test def compileTyperPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/typer", defaultOptions).pos

  @Test def compileUtilPos: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/util", defaultOptions).pos

  @Test def compileIoPos: Unit =
    compileDir("../compiler/src/dotty/tools/io", defaultOptions).pos

  @Test def labelsPos: Unit =
    compileFile("../tests/pos/Labels.scala", defaultOptions).times(2).pos

  @Test def testNonCyclic: Unit = compileList(
    List(
      "../compiler/src/dotty/tools/dotc/CompilationUnit.scala",
      "../compiler/src/dotty/tools/dotc/core/Types.scala",
      "../compiler/src/dotty/tools/dotc/ast/Trees.scala"
    ),
    defaultOptions.and("-Xprompt")
  ).times(2).pos

  @Test def issue34Pos: Unit = compileList(
    List(
      "../compiler/src/dotty/tools/dotc/config/Properties.scala",
      "../compiler/src/dotty/tools/dotc/config/PathResolver.scala"
    ),
    defaultOptions.and("-Xprompt")
  ).times(2).pos

  @Test def javaInteropPos: Unit =
    compileFilesInDir("../tests/pos-java-interop", defaultOptions).times(2).pos

  // New tests -----------------------------------------------------------------

  @Test def compileNew: Unit =
    compileFilesInDir("../tests/new", defaultOptions).pos

  // Negative tests ------------------------------------------------------------

  @Test def compileNeg: Unit =
    compileShallowFilesInDir("../tests/neg", defaultOptions).neg

  @Test def typedIdentsNeg: Unit =
    compileDir("../tests/neg/typedIdents", defaultOptions).neg

  @Test def typersNeg: Unit =
    compileFile("../tests/neg/customArgs/typers.scala", allowDoubleBindings).neg

  @Test def overrideClassNeg: Unit =
    compileFile("../tests/neg/customArgs/overrideClass.scala", scala2Mode).neg

  @Test def autoTuplingNeg: Unit =
    compileFile("../tests/neg/customArgs/autoTuplingTest.scala", defaultOptions.and("-language:noAutoTupling")).neg

  @Test def i1050Neg: Unit =
    compileFile("../tests/neg/customArgs/i1050.scala", defaultOptions.and("-strict")).neg

  @Test def i1240Neg: Unit =
    compileFile("../tests/neg/customArgs/i1240.scala", allowDoubleBindings).neg

  @Test def i2002Neg: Unit =
    compileFile("../tests/neg/customArgs/i2002.scala", allowDoubleBindings).neg

  @Test def nopredefNeg: Unit =
    compileFile("../tests/neg/customArgs/nopredef.scala", defaultOptions.and("-Yno-predef")).neg

  @Test def noimportsNeg: Unit =
    compileFile("../tests/neg/customArgs/noimports.scala", defaultOptions.and("-Yno-imports")).neg

  @Test def noimports2Neg: Unit =
    compileFile("../tests/neg/customArgs/noimports2.scala", defaultOptions.and("-Yno-imports")).neg

  @Test def t1672bTailcallNeg: Unit =
    compileFile("../tests/neg/tailcall/t1672b.scala", defaultOptions).neg

  @Test def t3275TailcallNeg: Unit =
    compileFile("../tests/neg/tailcall/t3275.scala", defaultOptions).neg

  @Test def t6574TailcallNeg: Unit =
    compileFile("../tests/neg/tailcall/t6574.scala", defaultOptions).neg

  @Test def tailrecTailcallNeg: Unit =
    compileFile("../tests/neg/tailcall/tailrec.scala", defaultOptions).neg

  @Test def tailrec2TailcallNeg: Unit =
    compileFile("../tests/neg/tailcall/tailrec-2.scala", defaultOptions).neg

  @Test def tailrec3TailcallNeg: Unit =
    compileFile("../tests/neg/tailcall/tailrec-3.scala", defaultOptions).neg

  // Run tests -----------------------------------------------------------------

  @Test def runAll: Unit =
    compileFilesInDir("../tests/run", defaultOptions).run

  @Test def runArraycopy: Unit =
    compileFile("../tests/run/arraycopy.scala", defaultOptions).run

  // Benchmark Tests -----------------------------------------------------------

  @Test def t2168Pos: Unit =
    compileFile("../tests/pos/t2168.scala", defaultOptions).times(2).pos

  @Test def erasurePos: Unit =
    compileFile("../tests/pos/erasure.scala", defaultOptions).times(2).pos

  @Test def coderPos: Unit =
    compileFile("../tests/pos/Coder.scala", defaultOptions).times(2).pos

  @Test def blockescapesPos: Unit =
    compileFile("../tests/pos/blockescapes.scala", defaultOptions).times(2).pos

  @Test def collectionsPos: Unit =
    compileFile("../tests/pos/collections.scala", defaultOptions).times(2).pos

  @Test def functions1Pos: Unit =
    compileFile("../tests/pos/functions1.scala", defaultOptions).times(2).pos

  @Test def implicits1Pos: Unit =
    compileFile("../tests/pos/implicits1.scala", defaultOptions).times(2).pos

  @Test def inferredPos: Unit =
    compileFile("../tests/pos/inferred.scala", defaultOptions).times(2).pos

  @Test def patternsPos: Unit =
    compileFile("../tests/pos/Patterns.scala", defaultOptions).times(2).pos

  @Test def selftypesPos: Unit =
    compileFile("../tests/pos/selftypes.scala", defaultOptions).times(2).pos

  @Test def varargsPos: Unit =
    compileFile("../tests/pos/varargs.scala", defaultOptions).times(2).pos

  @Test def varargPatternsPos: Unit =
    compileFile("../tests/pos/vararg-pattern.scala", defaultOptions).times(2).pos

  @Test def opassignPos: Unit =
    compileFile("../tests/pos/opassign.scala", defaultOptions).times(2).pos

  @Test def typedapplyPos: Unit =
    compileFile("../tests/pos/typedapply.scala", defaultOptions).times(2).pos

  @Test def nameddefaultsPos: Unit =
    compileFile("../tests/pos/nameddefaults.scala", defaultOptions).times(2).pos

  @Test def desugarPos: Unit =
    compileFile("../tests/pos/desugar.scala", defaultOptions).times(2).pos

  @Test def sigsPos: Unit =
    compileFile("../tests/pos/sigs.scala", defaultOptions).times(2).pos

  @Test def typersPos: Unit =
    compileFile("../tests/pos/typers.scala", defaultOptions).times(2).pos

  @Test def typedIdentsPos: Unit =
    compileDir("../tests/pos/typedIdents", defaultOptions).times(2).pos

  @Test def assignmentsPos: Unit =
    compileFile("../tests/pos/assignments.scala", defaultOptions).times(2).pos

  @Test def packageobjectPos: Unit =
    compileFile("../tests/pos/packageobject.scala", defaultOptions).times(2).pos

  @Test def overloadedPos: Unit =
    compileFile("../tests/pos/overloaded.scala", defaultOptions).times(2).pos

  @Test def overridesPos: Unit =
    compileFile("../tests/pos/overrides.scala", defaultOptions).times(2).pos

  @Test def javaOverridePos: Unit =
    compileDir("../tests/pos/java-override", defaultOptions).times(2).pos

  @Test def templateParentsPos: Unit =
    compileFile("../tests/pos/templateParents.scala", defaultOptions).times(2).pos

  @Test def overloadedAccessPos: Unit =
    compileFile("../tests/pos/overloadedAccess.scala", defaultOptions).times(2).pos

  @Test def approximateUnionPos: Unit =
    compileFile("../tests/pos/approximateUnion.scala", defaultOptions).times(2).pos

  @Test def tailcallPos: Unit =
    compileFilesInDir("../tests/pos/tailcall", defaultOptions).times(2).pos

  @Test def valueclassesPos: Unit =
    compileShallowFilesInDir("../tests/pos/pos_valueclasses", defaultOptions).times(2).pos

  @Test def subtypingPos: Unit =
    compileFile("../tests/pos/subtyping.scala", defaultOptions).times(2).pos

  @Test def packageObjPos: Unit =
    compileFile("../tests/pos/i0239.scala", defaultOptions).times(2).pos

  @Test def anonClassSubtypingPos: Unit =
    compileFile("../tests/pos/anonClassSubtyping.scala", defaultOptions).times(2).pos

  @Test def extmethodsPos: Unit =
    compileFile("../tests/pos/extmethods.scala", defaultOptions).times(2).pos

  @Test def companionsPos: Unit =
    compileFile("../tests/pos/companions.scala", defaultOptions).times(2).pos

  // Pickling Tests ------------------------------------------------------------

  @Test def testPickling: Unit =
    compileFilesInDir("../tests/pickling", picklingOptions).pos

  @Test def testPicklingAst: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/ast", picklingOptions).pos

  @Test def testPicklingInf: Unit =
    compileFile("../tests/pos/pickleinf.scala", picklingOptions).pos

  @Test def tastyNew: Unit =
    compileFilesInDir("../tests/new", picklingOptions).pos

  @Test def tastyRuntime: Unit =
    compileDir("../library/src/dotty/runtime", picklingOptions).pos

  @Test def tastyTools: Unit =
    compileDir("../compiler/src/dotty/tools", picklingOptions).pos

  @Test def tastyBackendJvm: Unit =
    compileDir("../compiler/src/dotty/tools/backend/jvm", picklingOptions).pos

  @Test def tastyDotc: Unit =
    compileDir("../compiler/src/dotty/tools/dotc", picklingOptions).pos

  @Test def tastyDotcAst: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/ast", picklingOptions).pos

  @Test def tastyDotcConfig: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/config", picklingOptions).pos

  @Test def tastyCore: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core", picklingOptions).pos

  @Test def tastyClassfile: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core/classfile", picklingOptions).pos

  @Test def dotcTasty: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core/tasty", picklingOptions).pos

  @Test def tastyUnpickleScala2: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/core/unpickleScala2", picklingOptions).pos

  @Test def tastyParsing: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/parsing", picklingOptions).pos

  @Test def tastyPrinting: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/printing", picklingOptions).pos

  @Test def tastyRepl: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/repl", picklingOptions).pos

  @Test def tastyRewrite: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/rewrite", picklingOptions).pos

  @Test def tastyTransform: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/transform", picklingOptions).pos

  @Test def tastyTyper: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/typer", picklingOptions).pos

  @Test def tastyUtil: Unit =
    compileDir("../compiler/src/dotty/tools/dotc/util", picklingOptions).pos

  @Test def tastyIo: Unit =
    compileDir("../compiler/src/dotty/tools/io", picklingOptions).pos

  @Test def tastyTests: Unit =
    compileDir("../tests/tasty", picklingOptions).pos

  @Test def tastyBootstrap: Unit = {
    def dotty1 =
      compileDir("../compiler/src/dotty", allowDeepSubtypes.and("-Ycheck-reentrant", "-strict"))
    def lib =
      compileDir("../library/src", defaultOptions)
    def dotty2 =
      compileDir("../compiler/src/dotty", defaultOptions.and("-priorityclasspath", defaultOutputDir))

    List(dotty1, lib, dotty2).map(_.keepOutput.pos).foreach(_.delete())
  }

  @Test def dotty = {
    def bootedLib =
      compileDir("../library/src", allowDeepSubtypes.and("-Ycheck-reentrant", "-strict"))
    def dottyDependsOnBootedLib =
      compileDir("../compiler/src/dotty", allowDeepSubtypes.and("-Ycheck-reentrant", "-strict"))

    List(bootedLib, dottyDependsOnBootedLib).map(_.keepOutput.pos).foreach(_.delete())
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
    "-pagewidth", "120"
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
