package dotty.tools.scaladoc
package snippets

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.fromtasty.TastyFileUtil
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.AbstractFile

class SnippetChecker(val args: Scaladoc.Args)(using cctx: CompilerContext):
  private val sep = System.getProperty("path.separator")

  private val fullClasspath = List(
    args.tastyFiles
      .map(_.getAbsolutePath())
      .map(AbstractFile.getFile(_))
      .flatMap(t => try TastyFileUtil.getClassPath(t) catch case _: AssertionError => Seq.empty)
      .distinct
      .mkString(sep),
    args.classpath
  ).mkString(sep)

  private val snippetCompilerSettings: Seq[SnippetCompilerSetting[?]] =
    val userSetSettings =
      cctx.settings.userSetSettings(cctx.settingsState)
        .filter(_ != cctx.settings.classpath)
        .map[SnippetCompilerSetting[?]]: setting =>
          SnippetCompilerSetting(setting, setting.valueIn(cctx.settingsState))
    userSetSettings :+ SnippetCompilerSetting(cctx.settings.classpath, fullClasspath)

  private val compiler: SnippetCompiler = SnippetCompiler(snippetCompilerSettings = snippetCompilerSettings)

  def checkSnippet(
    snippet: SnippetSource,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    sourceFile: SourceFile,
    sourceColumnOffset: Int
  ): Option[SnippetCompilationResult] =
    if arg.flag != SCFlags.NoCompile then
      val baseLineOffset = data.fold(0)(_.position.line)
      val baseColumnOffset = data.fold(0)(_.position.column) + sourceColumnOffset
      val sourceLines = snippet.sourceLines.map(_.map(_ + baseLineOffset))
      val adjustedSnippet = snippet.copy(
        sourceLines = sourceLines,
        outerLineOffset = snippet.outerLineOffset + baseLineOffset
      )
      val wrapped = WrappedSnippet(
        snippet.snippet,
        data.map(_.packageName),
        snippet.outerLineOffset + baseLineOffset,
        baseColumnOffset,
        sourceLines
      )
      Some(compiler.compile(
        adjustedSnippet,
        wrapped,
        arg,
        sourceFile
      ))
    else
      None

object SnippetChecker:
  // The first line of snippet content is two lines below the opening code fence.
  val codeFenceContentLineOffset = 2
  // Doc comments add ` * ` before snippet content, which shifts columns by four.
  val docCommentColumnOffset = 4

  type SnippetCheckingFunc = (SnippetSource, Option[SnippetCompilerArg]) => Option[SnippetCompilationResult]
