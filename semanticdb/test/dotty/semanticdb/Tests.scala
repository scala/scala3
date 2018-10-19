package dotty.semanticdb

import scala.tasty.Tasty
import scala.tasty.util.TreeTraverser
import scala.tasty.file._

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._

class Tests {

  // TODO: update scala-0.10 on version change (or resolve automatically)
  final def tastyClassDirectory = "out/bootstrap/dotty-semanticdb/scala-0.11/test-classes"
  val sourceroot = Paths.get("semanticdb", "input").toAbsolutePath
  val sourceDirectory = sourceroot.resolve("src/main/scala")

  val semanticdbClassDirectory = sourceroot.resolve("target/scala-2.12/classes")
  val semanticdbLoader = new Semanticdbs.Loader(sourceroot, List(semanticdbClassDirectory))
  /** Returns the SemanticDB for this Scala source file. */
  def getScalacSemanticdb(scalaFile: Path): s.TextDocument = {
    semanticdbLoader.resolve(scalaFile).get
  }

  /** TODO: Produce semanticdb from TASTy for this Scala source file. */
  def getTastySemanticdb(scalaFile: Path): s.TextDocument = {
    ???
  }

  /** Fails the test if the s.TextDocument from tasty and semanticdb-scalac are not the same. */
  def checkFile(filename: String): Unit = {
    val path = sourceDirectory.resolve(filename)
    val scalac = getScalacSemanticdb(path)
    val tasty = s.TextDocument(text = scalac.text) // TODO: replace with `getTastySemanticdb(path)`
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
        difflib.DiffUtils.generateUnifiedDiff(
          "tasty", "scala2", obtainedLines, patch, 1
        ).asScala.mkString("\n")
      }
    if (!diff.isEmpty) {
      fail("\n" + diff)
    }
  }


  @Test def testExample(): Unit = checkFile("example/Example.scala")
  // TODO: add more tests

  def testOutput(className: String, expected: String): Unit = {
    val out = new StringBuilder
    ConsumeTasty(tastyClassDirectory, List(className), new SemanticdbConsumer {
      override def println(x: Any): Unit = out.append(x).append(";")
    })
    assertEquals(expected, out.result())
  }
}
