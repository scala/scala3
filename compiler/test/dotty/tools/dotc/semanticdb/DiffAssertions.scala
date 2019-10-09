package dotty.tools.dotc.semanticdb

import scala.collection.JavaConverters._
import org.junit.Assert._

object DiffAssertions {
  def assertNoDiff(obtained: String, expected: String): Unit =
    val diff = compareContents(obtained, expected)
    if diff.nonEmpty then
      fail(diff)

  private def stripTrailingWhitespace(str: String): String = str.replaceAll(" \n", "∙\n")

  private def splitIntoLines(string: String): Seq[String] =
    string.trim.replace("\r\n", "\n").split("\n").toSeq

  private def compareContents(original: String, revised: String): String =
    compareContents(splitIntoLines(original), splitIntoLines(revised))

  private def compareContents(original: Seq[String], revised: Seq[String]): String = {
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "obtained",
          "expected",
          original.asJava,
          diff,
          1
        )
        .asScala
        .mkString("\n")
  }
}