package dotty.tools.vulpix

import scala.io.Source
import scala.util.Using

import java.io.File
import java.lang.System.{lineSeparator => EOL}
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets


object FileDiff {
  def diffMessage(expectFile: String, actualFile: String): String =
      s"""Test output dumped in: $actualFile
          |  See diff of the checkfile (`brew install icdiff` for colored diff)
          |    > diff $expectFile $actualFile
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

  def matches(actual: String, expect: String): Boolean = {
      val actual1 = actual.stripLineEnd
      val expect1  = expect.stripLineEnd

      // handle check file path mismatch on windows
      actual1 == expect1 || File.separatorChar == '\\' && actual1.replace('\\', '/') == expect1
  }

  def matches(actual: Seq[String], expect: Seq[String]): Boolean = {
    actual.length == expect.length
    && actual.lazyZip(expect).forall(matches)
  }

  def dump(path: String, content: Seq[String]): Unit = {
    val outFile = dotty.tools.io.File(path)
    outFile.writeAll(content.mkString("", EOL, EOL))
  }

  def checkAndDump(sourceTitle: String, actualLines: Seq[String], checkFilePath: String): Boolean = {
    val outFilePath = checkFilePath + ".out"
    FileDiff.check(sourceTitle, actualLines, checkFilePath) match {
      case Some(msg) =>
        FileDiff.dump(outFilePath, actualLines)
        println(msg)
        println(FileDiff.diffMessage(checkFilePath, outFilePath))
        false
      case _ =>
        val jOutFilePath = Paths.get(outFilePath)
        Files.deleteIfExists(jOutFilePath)
        true
    }
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
