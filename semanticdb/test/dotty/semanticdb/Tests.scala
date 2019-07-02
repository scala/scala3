package dotty.semanticdb

import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

import dotty.semanticdb.Scala._
import dotty.tools.dotc.util.SourceFile
import scala.math._

class Tests {

  def distance(r1: s.Range, sourceFile: SourceFile)(r2: s.Range): Int = {
    val s1 = sourceFile.lineToOffset(max(r1.startLine, 0)) + r1.startCharacter
    val s2 = sourceFile.lineToOffset(max(r2.startLine, 0)) + r2.startCharacter
    val e1 = sourceFile.lineToOffset(max(r1.endLine, 0)) + r1.endCharacter
    val e2 = sourceFile.lineToOffset(max(r2.endLine, 0)) + r2.endCharacter
    max(abs(s1 - s2), abs(e1 - e2))
  }

  /* This is a most powerfull way to compare two lists of occurences
  than just to print the diff. The diff will fail local symbols have not
  the same numer (even if their is no conflicts) and will also fail for symbols
  with small differences.
  The goal here is to have a method returning true if the tasty occurences and
  the scala occurences are the "same" (no local conflicts, positions can be slightly
  different). */
  def compareOccurrences(tastyOccurrences: Seq[s.SymbolOccurrence],
                         scalaOccurrences: Seq[s.SymbolOccurrence],
                         sourceCode: String): Boolean = {
    val sourceFile = SourceFile.virtual("", sourceCode)
    val symbols = tastyOccurrences.groupBy(_.symbol)
    val localTastyToScala = HashMap[String, String]()
    val localScalaToTasty = HashMap[String, String]()
    val translator = HashMap[(s.Range, String), s.SymbolOccurrence]()

    /* Check if we can translate the symbol [from] to the symbol [to]
       True if their is no local clash (ex if we know local2 -> local3 then local2 clash with local4)
       [from] is in tasty space, [to] in scala space
    */
    def checkIfTranslatableSymbol(from: String, to: String): Boolean = {
      if (from.isLocal != to.isLocal) {
        false
      } else {
        if (from.isLocal) {
          if (localTastyToScala.getOrElse(from, to) == to &&
              localScalaToTasty.getOrElse(to, from) == from) {
            localTastyToScala += (from -> to)
            localScalaToTasty += (to -> from)
            true
          } else {
            false
          }
        } else {
          true
        }
      }
    }

    /* If we do not have the same number of occurrences in both lists we know we failed
    the test */
    if (tastyOccurrences.length != scalaOccurrences.length) {
      false
    } else {
      /* We associate every scala occurrence to the nearest tasty occurence with the
      same name (if it is a global symbol) or local symbol */
      scalaOccurrences.forall(occurrence => {
        if (occurrence.symbol.isLocal ||
            symbols.contains(localScalaToTasty.getOrElse(occurrence.symbol,
                                                         occurrence.symbol))) {
          val siblings =
            if (occurrence.symbol.isLocal) tastyOccurrences
            else symbols(occurrence.symbol)

          val nearest = siblings.minBy(c =>
            distance(occurrence.range.get, sourceFile)(c.range.get))

          /* If these two symbols can be translated, meaning:
          - [nearest] was not associated with any other scala occurrence
          - if [occurrence] is a local symbol it does not clash with [nearest]
          - they are "near" in the source code */
          if (!checkIfTranslatableSymbol(nearest.symbol, occurrence.symbol) ||
              translator.contains((nearest.range.get, nearest.symbol)) ||
              distance(occurrence.range.get, sourceFile)(nearest.range.get) > 5) {
            println(
              checkIfTranslatableSymbol(nearest.symbol, occurrence.symbol))
            false
          } else {
            if (!occurrence.symbol.isLocal)
              translator += ((nearest.range.get, nearest.symbol) -> occurrence)
            true
          }
        } else {
          false
        }
      })
    }
  }

  final def tastyClassDirectory = {
    val root = "out/bootstrap/dotty-semanticdb/"
    val files = Paths.get(root).toFile().listFiles
    val scalaFolderReg = """scala-(\d+)\.(\d+)""".r
    val (_, _, path) = files
      .collect(file =>
        file.getName match {
          case scalaFolderReg(major, minor) => (major, minor, file.getName)
      })
      .max
    Paths.get(root, path, "test-classes")
  }

  val sourceroot = Paths.get("semanticdb/input").toAbsolutePath
  val sourceDirectory = sourceroot.resolve("src/main/scala")
  val semanticdbClassDirectory = sourceroot.resolve("target/scala-2.12/classes")
  val semanticdbLoader =
    new Semanticdbs.Loader(sourceroot, List(semanticdbClassDirectory))

  /** Returns the SemanticDB for this Scala source file. */
  def getScalacSemanticdb(scalaFile: Path): s.TextDocument = {
    semanticdbLoader.resolve(scalaFile).get
  }

  final def allTastyFiles = Utils.getTastyFiles(tastyClassDirectory, "example")

  /** Returns the SemanticDB for this Scala source file. */
  def getTastySemanticdb(classPath: Path, scalaFile: Path): s.TextDocument = {
    val classNames = Utils.getClassNamesCached(scalaFile, allTastyFiles)
    val sdbconsumer = new SemanticdbConsumer(scalaFile)

    val _ = ConsumeTasty(classPath.toString, classNames, sdbconsumer)
    sdbconsumer.toSemanticdb()
  }

  /** Fails the test if the s.TextDocument from tasty and semanticdb-scalac are not the same. */
  def checkFile(filename: String): Unit = {
    val path = sourceDirectory.resolve(filename)
    val scalac = getScalacSemanticdb(path)
    val tasty = getTastySemanticdb(tastyClassDirectory, path)
    val obtained = Semanticdbs.printTextDocument(tasty)
    val expected = Semanticdbs.printTextDocument(scalac)
    if (!compareOccurrences(tasty.occurrences, scalac.occurrences, scalac.text))
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

  @Test def testAccess(): Unit = checkFile("example/Access.scala")
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
  @Test def testMultiArguments(): Unit =
    checkFile("example/MultiArguments.scala")
  @Test def testObjects(): Unit = checkFile("example/Objects.scala")
  @Test def testOverrides(): Unit = checkFile("example/Overrides.scala")
  @Test def testPrefixes(): Unit = checkFile("example/Prefixes.scala")
  @Test def testSelfs(): Unit = checkFile("example/Selfs.scala")
  @Test def testSelfUse(): Unit = checkFile("example/SelfUse.scala")
  @Test def testTraits(): Unit = checkFile("example/Traits.scala")
  @Test def testTypes(): Unit = checkFile("example/Types.scala")
  @Test def testTypesAnnotations(): Unit =
    checkFile("example/TypesAnnotations.scala")
  @Test def testVals(): Unit = checkFile("example/Vals.scala")
  @Test def testDependantModule(): Unit =
    checkFile("example/DependantModule.scala")
  @Test def testNew(): Unit = checkFile("example/New.scala")
  @Test def testIgnoredSymbol(): Unit = checkFile("example/IgnoredSymbol.scala")
  @Test def testCase(): Unit = checkFile("example/Case.scala")
  @Test def testApply(): Unit = checkFile("example/Apply.scala")
  @Test def testMethodUsages(): Unit = checkFile("example/MethodUsages.scala")
  @Test def testSuper(): Unit = checkFile("example/Super.scala")
  @Test def testTypeBug(): Unit = checkFile("example/TypeBug.scala")
  @Test def testSynthetic(): Unit = checkFile("example/Synthetic.scala")
  @Test def testBinaryOp(): Unit = checkFile("example/BinaryOp.scala")
  @Test def testDottyPredef(): Unit = checkFile("example/DottyPredef.scala")

}
