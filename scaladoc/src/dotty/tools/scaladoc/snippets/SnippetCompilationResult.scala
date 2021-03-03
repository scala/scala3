package dotty.tools.scaladoc
package snippets

import dotty.tools.io.{ AbstractFile }

case class SnippetCompilerMessage(line: Int, column: Int, sourceLine: String, message: String)

case class SnippetCompilationResult(result: Option[AbstractFile], messages: Seq[SnippetCompilerMessage]):
  def getSummary: String = messages.map(m => s"At ${m.line}:${m.column}:\n${m.sourceLine}Compiler: ${m.message}").mkString("\n")