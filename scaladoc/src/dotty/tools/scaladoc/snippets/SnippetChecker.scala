package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext

class SnippetChecker(
  private val compiler: SnippetCompiler = SnippetCompiler(),
  private val wrapper: SnippetWrapper = SnippetWrapper()
):
  var warningsCount = 0
  var errorsCount = 0

  def checkSnippet(snippet: String, data: Option[SnippetCompilerData]): SnippetCompilationResult = {
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
    res
  }

  def summary: String = s"""
  |Snippet compiler summary:
  |  Found $warningsCount warnings
  |  Found $errorsCount errors
  |""".stripMargin