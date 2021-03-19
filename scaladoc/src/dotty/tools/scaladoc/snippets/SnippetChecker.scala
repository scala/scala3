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
  private val wrapper: SnippetWrapper = SnippetWrapper()
  var warningsCount = 0
  var errorsCount = 0

  def checkSnippet(
    snippet: String,
    data: Option[SnippetCompilerData],
    arg: SnippetCompilerArg
  ): Option[SnippetCompilationResult] = {
    if arg.is(SCFlags.Compile) then
      val wrapped = wrapper.wrap(
        snippet,
        data.map(_.packageName),
        data.flatMap(_.classType),
        data.flatMap(_.classGenerics),
        data.map(_.imports).getOrElse(Nil)
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