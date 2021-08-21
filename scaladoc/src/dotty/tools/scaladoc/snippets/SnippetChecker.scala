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

class SnippetChecker(val args: Scaladoc.Args)(using cctx: CompilerContext):

// (val classpath: String, val bootclasspath: String, val tastyFiles: Seq[File], isScalajs: Boolean, useJavaCp: Boolean):
  private val sep = System.getProperty("path.separator")

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
  private val constantLineOffset = 3
  private val constantColumnOffset = 4

  def checkSnippet(
    snippet: String,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    lineOffset: SnippetChecker.LineOffset,
    sourceFile: SourceFile
  ): Option[SnippetCompilationResult] = {
    if arg.flag != SCFlags.NoCompile then
      val wrapped = WrappedSnippet(
        snippet,
        data.map(_.packageName),
        data.fold(Nil)(_.classInfos),
        data.map(_.imports).getOrElse(Nil),
        lineOffset + data.fold(0)(_.position.line) + constantLineOffset,
        data.fold(0)(_.position.column) + constantColumnOffset
      )
      val res = compiler.compile(wrapped, arg, sourceFile)
      Some(res)
    else None
  }

object SnippetChecker:
  type LineOffset = Int
  type SnippetCheckingFunc = (String, LineOffset, Option[SCFlags]) => Option[SnippetCompilationResult]
