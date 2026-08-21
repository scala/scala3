package dotty.tools.scaladoc
package tasty.comments
package markdown

import com.vladsch.flexmark.ast.HtmlInline
import com.vladsch.flexmark.ext.footnotes.Footnote
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.parser.block.{NodePostProcessor, NodePostProcessorFactory}
import com.vladsch.flexmark.util.ast.{Document, Node, NodeTracker}
import com.vladsch.flexmark.util.data.MutableDataHolder
import com.vladsch.flexmark.util.sequence.BasedSequence

/** Inserts a separator between directly adjacent footnote references
 *  (e.g. `text[^1][^2]`), so that they do not render as a single run of
 *  superscript digits. This cannot be done in CSS: sibling selectors ignore
 *  the text between elements, so `sup + sup` would also match references
 *  that are words apart within the same paragraph.
 */
object FootnoteSeparatorExtension extends Parser.ParserExtension:
  val separator = """<sup class="footnote-ref-sep">,</sup>"""

  class Processor extends NodePostProcessor:
    def process(state: NodeTracker, node: Node): Unit = node match
      case f: Footnote if f.getPrevious.isInstanceOf[Footnote] =>
        val sep = HtmlInline(BasedSequence.of(separator))
        f.insertBefore(sep)
        state.nodeAdded(sep)
      case _ => ()

  object Factory extends NodePostProcessorFactory(false):
    addNodes(classOf[Footnote])
    override def apply(document: Document): NodePostProcessor = Processor()

  def parserOptions(opt: MutableDataHolder): Unit = () // noop
  override def extend(parserBuilder: Parser.Builder): Unit =
    parserBuilder.postProcessorFactory(Factory)
