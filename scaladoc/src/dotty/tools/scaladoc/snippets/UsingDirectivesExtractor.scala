package dotty.tools.scaladoc
package snippets

import collection.JavaConverters._
import scala.util.chaining._

import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.fromtasty.TastyFileUtil
import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.{ ScalaSettings, AllScalaSettings }


import com.virtuslab.using_directives._
import com.virtuslab.using_directives.custom.model._
import SnippetChecker._

import com.virtuslab.using_directives._
import com.virtuslab.using_directives.custom.model._
import com.virtuslab.using_directives.custom.utils.{ Position => UDPosition }
import com.virtuslab.using_directives.reporter._

object UsingDirectivesExtractor:

  type Result = (String, Seq[Import], Seq[SnippetCompilerSetting[_]], Seq[SnippetCompilerMessage])

  def processSnippet(snippet: String, imports: Seq[Import], outerLineOffset: Int, outerColumnOffset: Int, sourceFile: SourceFile): Result =
    val processor = UsingDirectivesProcessor()
    val udReporter = CustomUsingDirectivesReporter(sourceFile, outerLineOffset, outerColumnOffset)
    processor.getContext.setReporter(udReporter)
    def extractUsingDirectives(snippet: String): UsingDirectives =
      processor.extract(snippet.toCharArray)

    val importsWithUsingDirectives: Seq[(Import, UsingDirectives)] = imports.map(i => i -> extractUsingDirectives(i.snippet))
    val truncatedImports = importsWithUsingDirectives.map {
      case (i: Import, ud: UsingDirectives) => i.copy(snippet = i.snippet.substring(ud.getCodeOffset()))
    }
    val importUds = importsWithUsingDirectives.map(_(1))
    val snippetUd = extractUsingDirectives(snippet)
    val truncatedSnippet = snippet.substring(snippetUd.getCodeOffset())
    val mergedUsingDirectives = mergeUsingDirectives(importUds :+ snippetUd)

    val (settings, parsingMessage) = extractSettings(mergedUsingDirectives, outerLineOffset, sourceFile)

    (truncatedSnippet, truncatedImports, settings, udReporter.getMessages ++ parsingMessage)




  // Probably in the future we need to support more settings
  private def extractSettings(uds: Map[Path, List[Value[_]]], outerLineOffset: Int, sourceFile: SourceFile): (Seq[SnippetCompilerSetting[_]], Option[SnippetCompilerMessage]) = uds.flatMap {
    case (path, value) if path.getPath.asScala.toList == List("compiler", "setting") => value
    case (path, value) => Nil
  }.map(processValue).toList.pipe { settings =>
    val allScalaSettings = new SettingGroup with AllScalaSettings
    val argsSummary = allScalaSettings.processArguments(settings, true)
    val settingsState = argsSummary.sstate
    val nonDefaultSettings = allScalaSettings.allSettings
      .filter(s => !s.isDefaultIn(settingsState))
    val messages = argsSummary.warnings ++ argsSummary.errors

    (
      nonDefaultSettings.map(s => SnippetCompilerSetting(s, s.valueIn(settingsState))),
      Option.when(messages.nonEmpty)(createParsingReportMessage(messages, outerLineOffset, sourceFile))
    )
  }

  private def processValue(v: Value[_]): String = v.toString()

  private def mergeUsingDirectives(uds: Seq[UsingDirectives]): Map[Path, List[Value[_]]] = {
    import collection.JavaConverters._
    val maps = uds.map(_.getFlattenedMap.asScala.map {
      case (key, value) => key -> value.asScala.toList
    }.toMap)
    maps.reduceLeft {
      case (map1, map2) => map1 ++ map2
    }
  }

  private def createParsingReportMessage(msgs: Seq[String], lineOffset: Int, sourceFile: SourceFile): SnippetCompilerMessage = {
    import dotty.tools.dotc.util.Spans._
    val offsetFromLine = sourceFile.lineToOffset(lineOffset - 1)
    val span = Span(offsetFromLine, offsetFromLine)
    val pos = Some(Position(dotty.tools.dotc.util.SourcePosition(sourceFile, span), 0))
    SnippetCompilerMessage(pos, msgs.mkString("\n"), MessageLevel.Warning)
  }

  private class CustomUsingDirectivesReporter(sourceFile: SourceFile, outerLineOffset: Int, outerColumnOffset: Int) extends Reporter:
    import dotty.tools.dotc.util.Spans._

    private val builder = Seq.newBuilder[SnippetCompilerMessage]

    private def buildMessage(level: MessageLevel, msg: String, position: Option[UDPosition] = None) = {
      val pos = position.map { p =>
        val offsetFromLine = sourceFile.lineToOffset(p.getLine() + outerLineOffset - 1)
        val offsetFromColumn = p.getColumn()
        val span = Span(
          offsetFromLine + offsetFromColumn + outerColumnOffset,
          offsetFromLine + offsetFromColumn + outerColumnOffset
        )
        Position(dotty.tools.dotc.util.SourcePosition(sourceFile, span), 0)
      }
      SnippetCompilerMessage(pos, msg, level)
    }

    def getMessages: Seq[SnippetCompilerMessage] =
      builder.result()

    override def error(msg: String) = builder += buildMessage(MessageLevel.Warning, msg)

    override def warning(msg: String) = builder += buildMessage(MessageLevel.Warning, msg)

    override def error(pos: UDPosition, msg: String) = builder += buildMessage(MessageLevel.Warning, msg, Some(pos))

    override def warning(pos: UDPosition, msg: String) = builder += buildMessage(MessageLevel.Warning, msg, Some(pos))

    override def hasErrors(): Boolean = builder.knownSize != 0

    override def reset(): Unit = { } // We recreate reporter each time so its not needed to reset anything
