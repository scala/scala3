package dotty.tools
package repl

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.{Path, Files}
import java.util.Comparator

import org.junit.{Test, Ignore, BeforeClass, AfterClass}

import dotc.Driver
import dotc.reporting.TestReporter
import dotc.interfaces.Diagnostic.ERROR
import vulpix.{TestConfiguration, TestFlags}

/** Test that the REPL can shadow artifacts in the local filesystem on the classpath.
 *  Since the REPL launches with the current directory on the classpath, stray .class
 *  files containing definitions in the empty package will be in scope in the REPL.
 *  Additionally, any subdirectories will be treated as package names in scope.
 *  As this may come as a surprise to an unsuspecting user, we would like definitions
 *  from the REPL session to shadow these names.
 *
 *  Provided here is a framework for creating the filesystem artifacts to be shadowed
 *  and running scripted REPL tests with them on the claspath.
 */
object ShadowingTests:
  def classpath = TestConfiguration.basicClasspath + File.pathSeparator + shadowDir
  def options = ReplTest.commonOptions ++ Array("-classpath", classpath)
  def shadowDir = dir.toAbsolutePath.toString

  def createSubDir(name: String): Path =
    val subdir = dir.resolve(name)
    try Files.createDirectory(subdir)
    catch case _: java.nio.file.FileAlreadyExistsException =>
      assert(Files.isDirectory(subdir), s"failed to create shadowed subdirectory $subdir")
    subdir

  // The directory on the classpath containing artifacts to be shadowed
  private var dir: Path = null

  @BeforeClass def setupDir: Unit =
    dir = Files.createTempDirectory("repl-shadow")

  @AfterClass def tearDownDir: Unit =
    Files.walk(dir).sorted(Comparator.reverseOrder).forEach(Files.delete)
    dir = null

class ShadowingTests extends ReplTest(options = ShadowingTests.options):
  // delete contents of shadowDir after each test
  override def cleanup: Unit =
    super.cleanup
    val dir = ShadowingTests.dir
    Files.walk(dir)
      .filter(_ != dir)
      .sorted(Comparator.reverseOrder)
      .forEach(Files.delete)

  /** Run a scripted REPL test with the compilation artifacts of `shadowed` on the classpath */
  def shadowedScriptedTest(name: String, shadowed: String, script: String): Unit =
    compileShadowed(shadowed)
    testScript(name, script.linesIterator.toList)

  /** Compile the given source text and output to the shadow dir on the classpath */
  private def compileShadowed(src: String): Unit =
    val file: Path = Files.createTempFile("repl-shadow-test", ".scala")
    Files.write(file, src.getBytes)

    val flags =
      TestFlags(TestConfiguration.basicClasspath, TestConfiguration.noCheckOptions)
        .and("-d", ShadowingTests.shadowDir)
    val driver = new Driver
    val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
    driver.process(flags.all :+ file.toString, reporter)
    assert(!reporter.hasErrors, s"compilation of $file failed")
    Files.delete(file)
  end compileShadowed

  @Test def i7635 = shadowedScriptedTest(name = "<i7635>",
    shadowed = "class C(val c: Int)",
    script =
      """|scala> new C().c
         |-- Error: ----------------------------------------------------------------------
         |1 | new C().c
         |  | ^^^^^^^
         |  | missing argument for parameter c of constructor C in class C: (c: Int): C
         |1 error found
         |
         |scala> new C(13).c
         |val res0: Int = 13
         |
         |scala> class C { val c = 42 }
         |// defined class C
         |
         |scala> new C().c
         |val res1: Int = 42
         |""".stripMargin
  )

  @Test def `shadow subdirectories on classpath` =
    // NB: Tests of shadowing of subdirectories on the classpath are only valid
    // when the subdirectories exist prior to initialization of the REPL driver.
    // In the tests below this is enforced by the call to `testScript` which
    // in turn invokes `ReplDriver#resetToInitial`. When testing interactively,
    // the subdirectories may be created before launching the REPL, or during
    // an existing session followed by the `:reset` command.

    ShadowingTests.createSubDir("foo")
    testScript(name = "<shadow-subdir-foo>",
      """|scala> val foo = 3
         |val foo: Int = 3
         |
         |scala> foo
         |val res0: Int = 3
         |""".stripMargin.linesIterator.toList
    )

    ShadowingTests.createSubDir("x")
    testScript(name = "<shadow-subdir-x>",
      """|scala> val (x, y) = (42, "foo")
         |val x: Int = 42
         |val y: String = foo
         |
         |scala> if (true) x else y
         |val res0: Matchable = 42
         |""".stripMargin.linesIterator.toList
    )

    ShadowingTests.createSubDir("util")
    testScript(name = "<shadow-subdir-util>",
      """|scala> import util.Try
         |
         |scala> object util { class Try { override def toString = "you've gotta try!" }  }
         |// defined object util
         |
         |scala> import util.Try
         |scala> new Try
         |val res0: util.Try = you've gotta try!
         |""".stripMargin.linesIterator.toList
    )
end ShadowingTests
