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

  def checkSnippet(
    snippet: String,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg,
    lineOffset: SnippetChecker.LineOffset
  ): Option[SnippetCompilationResult] = {
    if arg.flag != SCFlags.NoCompile then
      val wrapped = WrappedSnippet(
        snippet,
        data.map(_.packageName),
        data.flatMap(_.classType),
        data.flatMap(_.classGenerics),
        data.map(_.imports).getOrElse(Nil),
        lineOffset + data.fold(0)(_.position.line) + 1,
        data.fold(0)(_.position.column)
      )
      val res = compiler.compile(wrapped, arg)
      Some(res)
    else None
  }

object SnippetChecker:
  type LineOffset = Int
  type SnippetCheckingFunc = (String, LineOffset, Option[SCFlags]) => Option[SnippetCompilationResult]
