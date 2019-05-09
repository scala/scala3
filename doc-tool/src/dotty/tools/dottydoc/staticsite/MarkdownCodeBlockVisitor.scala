package dotty.tools
package dottydoc
package staticsite

import com.vladsch.flexmark.ast._
import com.vladsch.flexmark.util.sequence.CharSubSequence

object MarkdownCodeBlockVisitor {
  def apply(node: Node): Unit =
    new NodeVisitor(
      new VisitHandler(classOf[FencedCodeBlock], new Visitor[FencedCodeBlock] {
        override def visit(node: FencedCodeBlock): Unit = {
          if (node.getOpeningMarker.length == 3)
            node.setOpeningMarker(CharSubSequence.of("```scala"))
        }
      })
    )
    .visit(node)
}