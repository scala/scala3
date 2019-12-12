package dotty.tools.dotc.semanticdb

import scala.collection.JavaConverters._

object DiffAssertions {
  def collectFailingDiff(obtained: String, expected: String, obtainedPath: String, expectedPath: String)(onFail: => Unit): Unit =
    val diff = compareContents(obtained, expected, obtainedPath, expectedPath)
    if diff.nonEmpty then
      onFail

  private def stripTrailingWhitespace(str: String): String = str.replaceAll(" \n", "∙\n")

  private def splitIntoLines(string: String): Seq[String] =
    string.trim.replace("\r\n", "\n").split("\n").toSeq

  private def compareContents(original: String, revised: String, obtainedPath: String, expectedPath: String): String =
    compareContents(splitIntoLines(original), splitIntoLines(revised), obtainedPath, expectedPath)

  private def compareContents(original: Seq[String], revised: Seq[String], obtainedPath: String, expectedPath: String): String = {
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          obtainedPath,
          expectedPath,
          original.asJava,
          diff,
          1
        )
        .asScala
        .mkString("\n")
  }
}
