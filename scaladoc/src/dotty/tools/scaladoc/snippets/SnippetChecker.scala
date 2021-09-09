package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext
import java.nio.file.Paths
import java.io.File

import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.fromtasty.TastyFileUtil
import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.ScalaSettings

import com.virtuslab.using_directives._
import com.virtuslab.using_directives.custom.model._

class SnippetChecker(val args: Scaladoc.Args)(using cctx: CompilerContext):
  import SnippetChecker._

// (val classpath: String, val bootclasspath: String, val tastyFiles: Seq[File], isScalajs: Boolean, useJavaCp: Boolean):
  private val sep = System.getProperty("path.separator")

  private val usingDirectivesProcessor = {
    val processor = UsingDirectivesProcessor()
    // TODO: Implement custom reporter
    processor
  }

  private val fullClasspath = List(
    args.tastyFiles
      .map(_.getAbsolutePath())
      .map(AbstractFile.getFile(_))
      .flatMap(t => try { TastyFileUtil.getClassPath(t) } catch { case e: AssertionError => Seq() })
      .distinct.mkString(sep),
    args.classpath
  ).mkString(sep)

  private val snippetCompilerSettings: Seq[SnippetCompilerSetting[_]] = cctx.settings.userSetSettings(cctx.settingsState).filter(_ != cctx.settings.classpath).map( s =>
    SnippetCompilerSetting(s, s.valueIn(cctx.settingsState))
  ) :+ SnippetCompilerSetting(cctx.settings.classpath, fullClasspath)

  private val compiler: SnippetCompiler = SnippetCompiler(snippetCompilerSettings = snippetCompilerSettings)

  // These constants were found empirically to make snippet compiler
  // report errors in the same position as main compiler.
  private val constantLineOffset = 2
  private val constantColumnOffset = 4

  def checkSnippet(
    snippet: String,
    snippetImports: Seq[Import],
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    lineOffset: SnippetChecker.LineOffset,
    sourceFile: SourceFile
  ): Option[SnippetCompilationResult] = {
    val importsWithUsingDirectives: Seq[(Import, UsingDirectives)] = snippetImports.map(i => i -> extractUsingDirectives(i.snippet))
    val truncatedImports = importsWithUsingDirectives.map {
      case (i: Import, ud: UsingDirectives) => i.copy(snippet = i.snippet.substring(ud.getCodeOffset()))
    }
    val importUds = importsWithUsingDirectives.map(_(1))
    val snippetUd = extractUsingDirectives(snippet)
    val truncatedSnippet = snippet.substring(snippetUd.getCodeOffset())
    val mergedUsingDirectives = mergeUsingDirectives(importUds :+ snippetUd)

    val additionalCompilerSettings = UsingDirectivesSettingsExtractor.process(mergedUsingDirectives)

    val mergedSnippet = mergeSnippets(truncatedSnippet, truncatedImports)
    if arg.flag != SCFlags.NoCompile then
      val wrapped = WrappedSnippet(
        mergedSnippet,
        data.map(_.packageName),
        data.fold(Nil)(_.classInfos),
        data.map(_.imports).getOrElse(Nil),
        lineOffset + data.fold(0)(_.position.line) + constantLineOffset,
        data.fold(0)(_.position.column) + constantColumnOffset,
        additionalCompilerSettings
      )
      val res = compiler.compile(wrapped, arg, sourceFile)
      Some(res)
    else None
  }

  private def mergeSnippets(snippet: String, snippetImports: Seq[Import]) = {
    val wrappedImports: Seq[String] = snippetImports.map {
      case Import(id, s) =>
        s"""//{i:$id
          |$s
          |//i}""".stripMargin
    }
    wrappedImports :+ snippet
  }.mkString("\n")

  private def mergeUsingDirectives(uds: Seq[UsingDirectives]): Map[Path, Value[_]] = {
    import collection.JavaConverters._
    val maps = uds.map(_.getFlattenedMap.asScala.toMap)
    maps.reduceLeft {
      case (map1, map2) => map1 ++ map2
    }
  }

  private def extractUsingDirectives(snippet: String): UsingDirectives =
    usingDirectivesProcessor.extract(snippet.toCharArray)

object SnippetChecker:
  case class Import(id: String, snippet: String)
  type LineOffset = Int
  type SnippetCheckingFunc = (String, Seq[Import], LineOffset, Option[SCFlags]) => Option[SnippetCompilationResult]
