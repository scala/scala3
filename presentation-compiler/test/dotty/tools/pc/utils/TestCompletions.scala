package dotty.tools.pc.utils

import scala.language.unsafeNulls

import org.eclipse.lsp4j.CompletionItem

object TestCompletions:

  /** Returns the fully qualified label like "java.util.Dequeue" from the
   *  completion item.
   *
   *    - `item.getLabel` always contains unqualified names, even for "workspace
   *      completions" which insert fully qualified names.
   *    - `item.getInsertText` includes the fully qualied name of "workspace
   *      completions" and snippet syntax like `println($0)` or
   *      `java.util.Dequeue[$0]`.
   *
   *  This method returns the insert text without the snippet syntax.
   */
  def getFullyQualifiedLabel(item: CompletionItem): String =
    if item.getInsertText == null then
      item.getLabel
    else
      val idx = item.getInsertText.indexOf(item.getLabel)
      if idx < 0 then item.getLabel
      else
        val fullyQualifiedPrefix = item.getInsertText.substring(0, idx)
        fullyQualifiedPrefix + item.getLabel
