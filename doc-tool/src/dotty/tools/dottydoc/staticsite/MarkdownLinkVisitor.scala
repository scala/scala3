package dotty.tools
package dottydoc
package staticsite

import com.vladsch.flexmark.ast._

object MarkdownLinkVisitor {
  def apply(node: Node): Unit =
    (new NodeVisitor(
      new VisitHandler(classOf[Link], new Visitor[Link] {
        override def visit(node: Link): Unit = {
          val url = node.getUrl
          if (url.endsWith(".md")) node.setUrl {
            url.subSequence(0, url.lastIndexOf('.')).append(".html")
          }
        }
      })
    ))
    .visit(node)
}
