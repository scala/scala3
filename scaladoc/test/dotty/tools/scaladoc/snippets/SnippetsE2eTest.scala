package dotty.tools.scaladoc
package snippets

import scala.io.Source

import org.junit.Test
import org.junit.Assert._
import dotty.tools.io.{AbstractFile, VirtualDirectory}
import dotty.tools.scaladoc.test.BuildInfo
import scala.util.matching.Regex
import dotty.tools.dotc.reporting.{ Diagnostic, StoreReporter }

import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}
import com.vladsch.flexmark.formatter.Formatter
import com.vladsch.flexmark.util.options.MutableDataSet
import collection.JavaConverters._

import dotty.tools.scaladoc.tasty.comments.markdown.ExtendedFencedCodeBlock

abstract class SnippetsE2eTest(testName: String, flag: SCFlags) extends ScaladocTest(testName):

  import SnippetsE2eTest._

  val source = Source.fromFile(s"${BuildInfo.test_testcasesSourceRoot}/tests/$testName.scala")

  val snippetsCount = source.getLines.filter(_.indexOf("```scala") != -1).size

  def report(str: String) = s"""|In test $testName:
                                |$str""".stripMargin

  override def args = Scaladoc.Args(
      name = "test",
      tastyDirs = BuildInfo.test_testcasesOutputDir.map(java.io.File(_)).toSeq,
      tastyFiles = tastyFiles(testName),
      output = getTempDir().getRoot,
      projectVersion = Some("1.0"),
      snippetCompiler = List(s"${BuildInfo.test_testcasesSourceRoot}/tests=${flag.flagName}")
    )

  override def withModule(op: DocContext ?=> Module => Unit) =
    given DocContext = DocContext(args, testContext.fresh.setReporter(new StoreReporter))
    op(ScalaModuleProvider.mkModule())

  private def checkWrappedSnippet(ws: WrappedSnippet, si: SnippetInfo) = {
    assertTrue(
      report(
        s"Invalid outer line offset: ${ws.outerLineOffset}. " +
          s"Expected: ${si.outerOffset.line}\n"
      ),
      ws.outerLineOffset == si.outerOffset.line
    )
    assertTrue(
      report(
        s"Invalid outer column offset: ${ws.outerColumnOffset}. " +
          s"Expected: ${si.outerOffset.column}\n"
      ),
      ws.outerColumnOffset == si.outerOffset.column
    )
    assertTrue(
      report(
        s"Invalid inner line offset: ${ws.innerLineOffset}. " +
          s"Expected: ${si.innerOffset.line}\n"
      ),
      ws.innerLineOffset == si.innerOffset.line
    )
    assertTrue(
      report(
        s"Invalid inner column offset: ${ws.innerColumnOffset}. " +
          s"Expected: ${si.innerOffset.column}\n"
      ),
      ws.innerColumnOffset == si.innerOffset.column
    )
  }

  private def checkMessages(compilationMessages: Seq[SnippetCompilerMessage], messages: Seq[Message], ws: WrappedSnippet) = {
    val compilationMessagesWithPos = compilationMessages.collect {
      case m @ SnippetCompilerMessage(Some(_), _, _) => m
    }.toList
    def isSamePosition(msg: Message, cmsg: SnippetCompilerMessage): Boolean =
      cmsg.level == msg.level && cmsg.position.get.line == msg.offset.line && cmsg.position.get.column == msg.offset.column

    def checkRelativeLines(msg: Message, cmsg: SnippetCompilerMessage): Seq[String] =
      val pos = cmsg.position.get
      if !(pos.relativeLine == pos.line - ws.outerLineOffset + ws.innerLineOffset) then Seq(
        s"Expected ${msg.level.text} message at relative line: ${pos.line - ws.outerLineOffset + ws.innerLineOffset} " +
          s"but found at ${pos.relativeLine}"
      ) else Nil

    val mResult = compilationMessagesWithPos.flatMap { cmsg =>
      messages
        .find(msg => isSamePosition(msg, cmsg))
        .fold(Seq(s"Unexpected compilation message: ${cmsg.message} at relative line: ${cmsg.position.fold(-1)(_.line)}"))(_ => Seq())
    }

    val result = mResult ++ messages.flatMap { msg =>
      compilationMessagesWithPos
        .find(cmsg => isSamePosition(msg, cmsg))
        .fold(Seq(s"Expected ${msg.level.text} message at ${msg.offset.line}:${msg.offset.column}.")) { resp =>
          checkRelativeLines(msg, resp)
        }
    }

    if !result.isEmpty then {
      val errors = result.mkString("\n")
      val foundMessages = compilationMessages.map(m => s"${m.level} at ${m.position.get.line}:${m.position.get.column}").mkString("\n")
      throw AssertionError(Seq("Errors:", errors,"Found:", foundMessages).mkString("\n", "\n", "\n"))
    }
  }

  def moduleTestingFunc: DocContext ?=> Module => Unit = (m: Module) => {
    val snippets = m.members.values
      .flatMap(_.docs)
      .map(_.body)
      .collect { case n: mdu.Node => n }
      .flatMap(_.getDescendants.asScala)
      .collect { case en: ExtendedFencedCodeBlock => en }

    assertTrue(report(s"Expected $snippetsCount snippets but found ${snippets.size}"), snippets.size == snippetsCount)

    snippets.foreach { snippet =>
      val configStrs = (snippet.getPrevious() match {
        case c: mdu.ContentNode =>
          c.getContentChars.toString.split("\n").map(_.trim)
        case _ => throw AssertionError(s"Not found info for snippet ${snippet.codeBlock.getContentChars.toString}")
      }).toList
      val info = SnippetInfo(configStrs.head)
      val messages = configStrs.tail.map(Message.apply)
      val compilationResult = snippet.compilationResult match {
        case Some(res) => res
        case None => throw AssertionError(s"Snippet validation failed:\n${snippet.codeBlock.getContentChars.toString}")
      }
      val wrappedSnippet = compilationResult.wrappedSnippet
      checkWrappedSnippet(wrappedSnippet, info)
      checkMessages(compilationResult.messages, messages, wrappedSnippet)
    }
  }

  def runTest = {
    org.junit.Assume.assumeTrue("Running on Windows", java.io.File.separatorChar == '/')
    withModule(moduleTestingFunc)
  }

object SnippetsE2eTest:
  case class Offset(line: Int, column: Int)
  case class SnippetInfo(outerOffset: Offset, innerOffset: Offset)
  case class Message(level: MessageLevel, offset: Offset)
  object SnippetInfo:
    def apply(str: String): SnippetInfo = str match {
      case snippetInfoRegex(ol, oc, il, ic) => SnippetInfo(
        Offset(ol.toInt, oc.toInt),
        Offset(il.toInt, ic.toInt)
      )
    }

  object Message:
    def apply(str: String): Message = str match {
      case errorRegex(ln, cl) => Message(MessageLevel.Error, Offset(ln.toInt, cl.toInt))
      case warningRegex(ln, cl) => Message(MessageLevel.Warning, Offset(ln.toInt, cl.toInt))
    }
  val snippetInfoRegex = (raw"SNIPPET\(" +
    raw"OUTERLINEOFFSET:(\d+),OUTERCOLUMNOFFSET:(\d+)," +
    raw"INNERLINEOFFSET:(\d+),INNERCOLUMNOFFSET:(\d+)\)").r

  val warningRegex = raw"WARNING\(LINE:(\d+),COLUMN:(\d+)\)".r
  val errorRegex = raw"ERROR\(LINE:(\d+),COLUMN:(\d+)\)".r