package dotty.dokka

import org.junit.{Test, Rule}
import org.junit.Assert._
import org.junit.rules.ErrorCollector
import org.jetbrains.dokka.testApi.testRunner.AbstractCoreTest$TestBuilder
import scala.io.Source
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.pages.ContentNodesKt
import org.jetbrains.dokka._
import scala.jdk.CollectionConverters._
import scala.math.max

object MultipleFileTest{
  val classlikeKinds = Seq("class",  "object", "trait") // TODO add docs for packages
  val members = Seq("type", "def", "val", "var")
  val all = classlikeKinds ++ members
}

abstract class MultipleFileTest(val sourceFiles: List[String], val tastyFolders: List[String], signatureKinds: Seq[String], ignoreUndocumentedSignatures: Boolean = false
) extends DottyAbstractCoreTest:
  private val _collector = new ErrorCollector();

  // This should work correctly except for names in backticks and operator names containing a colon
  def extractSymbolName(signature: String) =
    val Pattern = s"""(?s).*(?:${signatureKinds.mkString("|")}) ([^\\[(: \\n\\t]+).*""".r
    signature match {
      case Pattern(name) => name
      case x => "NULL"
    }

  def matchSignature(s: String, signatureList: List[String]): Seq[String] =
    val symbolName = extractSymbolName(s)
    val candidates = signatureList.filter(extractSymbolName(_) == symbolName)

    candidates.filter(_ == s) match {
      case Nil =>
        val candidateMsg =
          if candidates.isEmpty then s"No candidate found for symbol name $symbolName"
          else s"Candidates:\n${candidates.mkString("\n")}\n"

        //reportError(s"No match for:\n$s\n$candidateMsg") All test would fail because of documented inherited methods
        //println(s"No match for:\n$s\n$candidateMsg")
        Nil
      case matching =>
        matching
    }

  @Test
  def testSignatures(): Unit =
    def cleanup(s: String) = s.replace("\n", " ").replaceAll(" +", " ")

    val allFromSource = sourceFiles.map{ file =>
      val all = signaturesFromSource(Source.fromFile(s"${BuildInfo.test_testcasesSourceRoot}/tests/$file.scala"))
      (all.expected, all.unexpected)
    }

    val expectedFromSource = allFromSource.map(_._1).flatten.filter(extractSymbolName(_) != "NULL").map(cleanup)
    val unexpectedFromSource = allFromSource.map(_._2).flatten.filter(extractSymbolName(_) != "NULL").map(cleanup)
    val unexpectedSignatureSymbolNames = unexpectedFromSource.map(extractSymbolName)

    val allFromDocumentation = tastyFolders.flatMap(folder => signaturesFromDocumentation(s"${BuildInfo.test_testcasesOutputDir}/tests/$folder"))
    val fromDocumentation = allFromDocumentation.filter(extractSymbolName(_) != "NULL").map(cleanup)

    val documentedSignatures = fromDocumentation.flatMap(matchSignature(_, expectedFromSource)).toSet
    val missingSignatures = expectedFromSource.filterNot(documentedSignatures.contains)

    val unexpectedSignatures =
      fromDocumentation.filter(s => unexpectedSignatureSymbolNames.contains(extractSymbolName(s))).toSet

    val reportMissingSignatures = !ignoreUndocumentedSignatures && missingSignatures.nonEmpty
    val reportUnexpectedSignatures = unexpectedSignatures.nonEmpty

    if reportMissingSignatures || reportUnexpectedSignatures then
      val missingSignaturesMessage = Option.when(reportMissingSignatures)
        (s"Not documented signatures:\n${missingSignatures.mkString("\n")}")

      val unexpectedSignaturesMessage = Option.when(reportUnexpectedSignatures)
        (s"Unexpectedly documented signatures:\n${unexpectedSignatures.mkString("\n")}")

      val allSignaturesMessage =
        s"""
        |All documented signatures:
        |${documentedSignatures.mkString("\n")}
        |
        |All expected signatures from source:
        |${expectedFromSource.mkString("\n")}
        """.stripMargin

      val errorMessages = missingSignaturesMessage ++ unexpectedSignaturesMessage ++ Some(allSignaturesMessage)

      reportError(errorMessages.mkString("\n", "\n\n", "\n"))
