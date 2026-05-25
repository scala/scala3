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
  private val sep = System.getProperty("path.separator")

  private val fullClasspath = List(
    args.tastyFiles
      .map(_.getAbsolutePath())
      .map(AbstractFile.getFile(_))
      .flatMap(t => try { TastyFileUtil.getClassPath(t) } catch { case e: AssertionError => Seq() })
      .distinct.mkString(sep),
    args.classpath
  ).mkString(sep)

  private val snippetCompilerSettings: Seq[SnippetCompilerSetting[?]] = cctx.settings.userSetSettings(cctx.settingsState).filter(_ != cctx.settings.classpath)
  .map[SnippetCompilerSetting[?]]( s =>
    SnippetCompilerSetting(s, s.valueIn(cctx.settingsState))
  ) :+ SnippetCompilerSetting(cctx.settings.classpath, fullClasspath)

  private val compiler: SnippetCompiler = SnippetCompiler(snippetCompilerSettings = snippetCompilerSettings)

  def checkSnippet(
    snippet: SnippetSource,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    sourceFile: SourceFile,
    sourceColumnOffset: Int
  ): Option[SnippetCompilationResult] = {
    if arg.flag != SCFlags.NoCompile then
      val baseLineOffset = data.fold(0)(_.position.line)
      val baseColumnOffset = data.fold(0)(_.position.column) + sourceColumnOffset
      val wrapped = WrappedSnippet(
        snippet.snippet,
        data.map(_.packageName),
        snippet.outerLineOffset + baseLineOffset,
        baseColumnOffset,
        snippet.sourceLines.map(_.map(_ + baseLineOffset))
      )
      Some(compiler.compile(wrapped, arg, sourceFile))
    else
      None

  }

object SnippetChecker:
  // The first line of snippet content is two lines below the opening code fence.
  val codeFenceContentLineOffset = 2
  // Doc comments add ` * ` before snippet content, which shifts columns by four.
  val docCommentColumnOffset = 4

  type SnippetCheckingFunc = (SnippetSource, Option[SnippetCompilerArg]) => Option[SnippetCompilationResult]
