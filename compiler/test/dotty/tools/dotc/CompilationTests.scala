package dotty
package tools
package dotc

import org.junit.Test
import java.io.{ File => JFile }

class CompilationTests extends ParallelTesting {
  import CompilationTests.{ defaultOutputDir, defaultOptions, picklingOptions }

  // Positive tests ------------------------------------------------------------

  @Test def compilePos =
    compileFilesInDir("../tests/pos", defaultOptions).pos

  // Negative tests ------------------------------------------------------------

  @Test def compileNeg =
    compileShallowFilesInDir("../tests/neg", defaultOptions).neg

  // Run tests -----------------------------------------------------------------

  @Test def runArraycopy =
    compileFile("../tests/run/arraycopy.scala", defaultOptions).run

  @Test def runAll =
    compileFilesInDir("../tests/run", defaultOptions).run

  // Pickling Tests ------------------------------------------------------------

  @Test def testPickling =
    compileFilesInDir("../tests/pickling", picklingOptions).pos

  @Test def testPicklingAst =
    compileFilesInDir("../compiler/src/dotty/tools/dotc/ast", picklingOptions).pos

  @Test def testPicklingInf =
    compileFile("../tests/pos/pickleinf.scala", picklingOptions).pos
}

object CompilationTests {
  implicit val defaultOutputDir: String = "../out/"

  private val noCheckOptions = Array(
    "-pagewidth", "80"
  )

  private val checkOptions = Array(
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-Yforce-sbt-phases"
  )

  private val classPath = {
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
}
