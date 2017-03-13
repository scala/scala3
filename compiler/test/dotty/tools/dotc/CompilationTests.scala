package dotty
package tools
package dotc

import org.junit.Test
import java.io.{ File => JFile }

class CompilationTests extends ParallelTesting {
  import CompilationTests.{ defaultOutputDir, defaultOptions }

  @Test def compilePos =
    compileFilesInDir("../tests/pos", defaultOptions).pos

  @Test def compileNeg =
    compileShallowFilesInDir("../tests/neg", defaultOptions).neg
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
}
