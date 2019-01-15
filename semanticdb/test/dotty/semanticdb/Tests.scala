package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

class Tests {

  // TODO: update scala-0.13 on version change (or resolve automatically)
  final def tastyClassDirectory =
    Paths.get("out/bootstrap/dotty-semanticdb/scala-0.12/test-classes/")

  val sourceroot = Paths.get("semanticdb/input").toAbsolutePath
  val sourceDirectory = sourceroot.resolve("src/main/scala")
  val semanticdbClassDirectory = sourceroot.resolve("target/scala-2.12/classes")
  val semanticdbLoader =
    new Semanticdbs.Loader(sourceroot, List(semanticdbClassDirectory))

  /** Returns the SemanticDB for this Scala source file. */
  def getScalacSemanticdb(scalaFile: Path): s.TextDocument = {
    semanticdbLoader.resolve(scalaFile).get
  }

  /** Returns the SemanticDB for this Scala source file. */
  def getTastySemanticdb(classPath: Path, scalaFile: Path) : s.TextDocument = {
    val classNames = Utils.getClassNames(classPath, scalaFile, "example/")
    println(classPath)
    println(classNames)
    println(scalaFile)
    val sdbconsumer = new SemanticdbConsumer(scalaFile)

    val _ = ConsumeTasty(classPath.toString, classNames, sdbconsumer)
    sdbconsumer.toSemanticdb()
  }

  /** Fails the test if the s.TextDocument from tasty and semanticdb-scalac are not the same. */
  def checkFile(filename: String): Unit = {
    val path = sourceDirectory.resolve(filename)
    val scalac = getScalacSemanticdb(path)
    val tasty = getTastySemanticdb(tastyClassDirectory, path)
    println(tasty)
    val obtained = Semanticdbs.printTextDocument(tasty)
    val expected = Semanticdbs.printTextDocument(scalac)
    assertNoDiff(obtained, expected)
  }

  /** Fails the test with a pretty diff if there obtained is not the same as expected */
  def assertNoDiff(obtained: String, expected: String): Unit = {
    if (obtained.isEmpty && !expected.isEmpty) fail("obtained empty output")
    def splitLines(string: String): java.util.List[String] =
      string.trim.replace("\r\n", "\n").split("\n").toSeq.asJava
    val obtainedLines = splitLines(obtained)
    val b = splitLines(expected)
    val patch = difflib.DiffUtils.diff(obtainedLines, b)
    val diff =
      if (patch.getDeltas.isEmpty) ""
      else {
        difflib.DiffUtils
          .generateUnifiedDiff(
            "tasty",
            "scala2",
            obtainedLines,
            patch,
            1
          )
          .asScala
          .mkString("\n")
      }
    if (!diff.isEmpty) {
      fail("\n" + diff)
    }
  }


  /*@Test def testAccess(): Unit = checkFile("example/Access.scala")
  @Test def testAdvanced(): Unit = checkFile("example/Advanced.scala")
  @Test def testAnonymous(): Unit = checkFile("example/Anonymous.scala")
  @Test def testClasses(): Unit = checkFile("example/Classes.scala")
  @Test def testEmpty(): Unit = checkFile("example/Empty.scala")
  @Test def testEmptyObject(): Unit = checkFile("example/EmptyObject.scala")
  @Test def testExample(): Unit = checkFile("example/Example.scala")
  @Test def testExample2(): Unit = checkFile("example/Example2.scala")
  @Test def testExclude(): Unit = checkFile("example/Exclude.scala")
  @Test def testFlags(): Unit = checkFile("example/Flags.scala")
  @Test def testIssue1749(): Unit = checkFile("example/Issue1749.scala")
  @Test def testLocalFile(): Unit = checkFile("example/local-file.scala")
  @Test def testLocals(): Unit = checkFile("example/Locals.scala")
  //deactivated @Test def testMacroAnnotations(): Unit = checkFile("example/MacroAnnotations.scala")
  @Test def testMethods(): Unit = checkFile("example/Methods.scala")
  @Test def testMultiArguments(): Unit = checkFile("example/MultiArguments.scala")
  @Test def testObjects(): Unit = checkFile("example/Objects.scala")
  @Test def testOverrides(): Unit = checkFile("example/Overrides.scala")
  @Test def testPrefixes(): Unit = checkFile("example/Prefixes.scala")
  @Test def testSelfs(): Unit = checkFile("example/Selfs.scala")
  @Test def testSelfUse(): Unit = checkFile("example/SelfUse.scala")
  @Test def testTraits(): Unit = checkFile("example/Traits.scala")
  @Test def testTypes(): Unit = checkFile("example/Types.scala")
  @Test def testTypesAnnotations() : Unit = checkFile("example/TypesAnnotations.scala") // Crash, has to deal with init symbols
  @Test def testVals(): Unit = checkFile("example/Vals.scala")
  @Test def testDependantModule(): Unit = checkFile("example/DependantModule.scala")
  @Test def testNew(): Unit = checkFile("example/New.scala")
  @Test def testIgnoredSymbol(): Unit = checkFile("example/IgnoredSymbol.scala")
  @Test def testCase(): Unit = checkFile("example/Case.scala")
  @Test def testApply(): Unit = checkFile("example/Apply.scala")
  @Test def testMethodUsages(): Unit = checkFile("example/MethodUsages.scala")
  @Test def testSuper(): Unit = checkFile("example/Super.scala")
  @Test def testTypeBug(): Unit = checkFile("example/TypeBug.scala")
  @Test def testSynthetic(): Unit = checkFile("example/Synthetic.scala")
  @Test def testBinaryOp(): Unit = checkFile("example/BinaryOp.scala") // Failure
  @Test def testDottyPredef(): Unit = checkFile("example/DottyPredef.scala")
  */

}
