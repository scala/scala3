package dotty.tools.scaladoc
package snippets

import dotty.tools.scaladoc.DocContext

class SnippetChecker(
  private val compiler: SnippetCompiler = SnippetCompiler(),
  private val wrapper: SnippetWrapper = SnippetWrapper()
):
  def checkSnippet(snippet: String): SnippetCompilationResult = {
    val wrapped = wrapper.wrap(snippet)
    compiler.compile(wrapped)
  }