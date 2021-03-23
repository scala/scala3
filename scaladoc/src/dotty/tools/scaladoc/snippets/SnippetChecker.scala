package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext
import java.nio.file.Paths

class SnippetChecker()(using ctx: DocContext):
  private val sep = System.getProperty("path.separator")
  private val cp = System.getProperty("java.class.path") + sep +
      Paths.get(ctx.args.classpath).toAbsolutePath + sep +
      ctx.args.tastyDirs.map(_.getAbsolutePath()).mkString(sep)
  private val compiler: SnippetCompiler = SnippetCompiler(classpath = cp)

  var warningsCount = 0
  var errorsCount = 0

  def checkSnippet(
    snippet: String,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    lineOffset: SnippetChecker.LineOffset
  ): Option[SnippetCompilationResult] = {
    if arg.is(SCFlags.Compile) then
      val wrapped = WrappedSnippet(
        snippet,
        data.map(_.packageName),
        data.flatMap(_.classType),
        data.flatMap(_.classGenerics),
        data.map(_.imports).getOrElse(Nil),
        lineOffset + data.fold(0)(_.position.line) + 1,
        data.fold(0)(_.position.column)
      )
      val res = compiler.compile(wrapped)
      if !res.messages.filter(_.level == MessageLevel.Error).isEmpty then errorsCount = errorsCount + 1
      if !res.messages.filter(_.level == MessageLevel.Warning).isEmpty then warningsCount = warningsCount + 1
      Some(res)
    else None
  }

  def summary: String = s"""
  |Snippet compiler summary:
  |  Found $warningsCount warnings
  |  Found $errorsCount errors
  |""".stripMargin

object SnippetChecker:
  type LineOffset = Int
  type SnippetCheckingFunc = (String, LineOffset, Option[SnippetCompilerArg]) => Unit
