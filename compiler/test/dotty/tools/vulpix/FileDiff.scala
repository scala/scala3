package dotty.tools.vulpix

import scala.language.unsafeNulls

import scala.io.Source
import scala.util.Using

import java.io.File
import java.lang.System.{lineSeparator => EOL}
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets


object FileDiff {
  def diffCommand(expectFile: String, actualFile: String): String =
    s"git diff --no-index -- $expectFile $actualFile"

  def diffMessage(expectFile: String, actualFile: String): String =
      s"""Test output dumped in: $actualFile
          |  See diff of the checkfile (`--color=always` for colored diff)
          |    > ${FileDiff.diffCommand(expectFile, actualFile)}
          |  Replace checkfile with current output
          |    > mv $actualFile $expectFile
      """.stripMargin

  def check(sourceTitle: String, outputLines: Seq[String], checkFile: String): Option[String] = {
    val checkLines =
      if (!(new File(checkFile)).exists) Nil
      else Using(Source.fromFile(checkFile, StandardCharsets.UTF_8.name))(_.getLines().toList).get

    if (!matches(outputLines, checkLines)) Some(
      s"""|Output from '$sourceTitle' did not match check file. Actual output:
          |${outputLines.mkString(EOL)}
          |""".stripMargin + "\n")
    else None
  }

  def matches(actual: String, expect: String): Boolean =
    val actual1 = actual.stripLineEnd
    val expect1 = expect.stripLineEnd
    // handle check file path mismatch on windows
    def matchesWindowsPath = File.separatorChar == '\\' && actual1.replace('\\', '/') == expect1
    // obscure line numbers in frames of stack trace output
    def obscureFrameLine(s: String): Option[String] =
      //at scala.quoted.runtime.impl.QuotesImpl$reflect$ClassDef$.module(QuotesImpl.scala:257)
      val frame = """\s+at [^(]+\([^:]+:(\d+)\)""".r
      frame.findFirstMatchIn(s).map(m => s"${m.before(1)}_${m.after(1)}")
    def matchesStackFrame =
      actual1.endsWith(")") && expect1.endsWith(")") && obscureFrameLine(actual1) == obscureFrameLine(expect1)
    actual1 == expect1 || matchesStackFrame || matchesWindowsPath

  def matches(actual: Seq[String], expect: Seq[String]): Boolean = actual.corresponds(expect)(matches)

  def dump(path: String, content: Seq[String]): Unit = {
    val outFile = dotty.tools.io.File(path)
    outFile.writeAll(content.mkString("", EOL, EOL))
  }

  def checkAndDumpOrUpdate(sourceTitle: String, actualLines: Seq[String], checkFilePath: String): Boolean = {
    val outFilePath = checkFilePath + ".out"
    FileDiff.check(sourceTitle, actualLines, checkFilePath) match {
      case Some(msg) if dotty.Properties.testsUpdateCheckfile =>
        Files.deleteIfExists(Paths.get(outFilePath))
        if actualLines.isEmpty
          then Files.deleteIfExists(Paths.get(checkFilePath))
          else FileDiff.dump(checkFilePath, actualLines)
        println("Updated checkfile: " + checkFilePath)
        true
      case Some(msg) =>
        FileDiff.dump(outFilePath, actualLines)
        println(msg)
        println(FileDiff.diffMessage(checkFilePath, outFilePath))
        false
      case _ =>
        Files.deleteIfExists(Paths.get(outFilePath))
        true
    }
  }
}
