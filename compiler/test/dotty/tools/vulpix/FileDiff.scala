package dotty.tools.vulpix

import scala.util.Properties.{javaSpecVersion, versionNumberString}
import scala.io.Codec
import dotty.tools.nio.*

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

  //at scala.quoted.runtime.impl.QuotesImpl$reflect$ClassDef$.module(QuotesImpl.scala:257)
  private val frame = """\s+at [^(]+\([^:]+:(\d+)\)""".r


  def check(sourceTitle: String, outputLines: Seq[String], checkFile: String, tolerateMissing: Boolean = true): Option[String] = {
    File.getOnDisk(checkFile) match
      case Some(f) =>
        var stacked = false
        val expected = f.readLines(Codec.UTF8)
        val actuals =
          if javaSpecVersion == "25" && versionNumberString.startsWith("3.8.") then
            outputLines.filter(!_.startsWith("WARNING:")) // ignore Unsafe warnings due to lazy vals
          else
            outputLines
        val matched =
          expected.corresponds(actuals): (expected, actual) =>
            matches(actual, expected) && {
              val framed = expected.endsWith(")") && frame.matches(expected)
              if framed then
                stacked = true
              !framed
            }
        if stacked then
          Some(s"Check file $checkFile includes a stack trace, which is brittle!")
        else if matched then
          None
        else
          // Do not use a """ literal here with .stripMargin since `outputLines` may begin with |
          Some(s"Output from '$sourceTitle' did not match check file. Actual output:\n${outputLines.mkString(System.lineSeparator())}\n")
      case None =>
        assert(tolerateMissing, "Missing check file: " + checkFile)
        None
  }

  def matches(actual: String, expect: String): Boolean = {
    val actual1 = actual.stripLineEnd
    val expect1 = expect.stripLineEnd
    def matchesWindowsPath = FileSystemEntry.separator == '\\' && actual1.replace('\\', '/') == expect1

    actual1 == expect1 || matchesWindowsPath // handle path mismatch on Windows
  }

  def matches(actual: Seq[String], expect: Seq[String]): Boolean = {
    actual.length == expect.length
    && actual.lazyZip(expect).forall(matches)
  }

  def dump(path: String, content: Seq[String]): Unit =
    File.getOrCreateOnDisk(path).writeLines(content, Codec.UTF8)

  def checkAndDumpOrUpdate(sourceTitle: String, actualLines: Seq[String], checkFilePath: String, tolerateMissingCheckFile: Boolean = true): Boolean = {
    val outFilePath = checkFilePath + ".out"
    FileDiff.check(sourceTitle, actualLines, checkFilePath, tolerateMissingCheckFile) match {
      case Some(msg) if dotty.Properties.testsUpdateCheckfile =>
        File.getOnDisk(outFilePath).foreach(_.delete())
        if actualLines.isEmpty
          then File.getOnDisk(checkFilePath).foreach(_.delete())
          else FileDiff.dump(checkFilePath, actualLines)
        println("Updated checkfile: " + checkFilePath)
        true
      case Some(msg) =>
        FileDiff.dump(outFilePath, actualLines)
        println(msg)
        println(FileDiff.diffMessage(checkFilePath, outFilePath))
        false
      case _ =>
        File.getOnDisk(outFilePath).foreach(_.delete())
        true
    }
  }
}
