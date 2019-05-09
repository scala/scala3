package dotty.tastydoc
package comment

/** The `MarkdownShortener` takes a node and *mutates* it and all children so
  * that the displayed length of the generated HTML doesn't exceeed `maxLen`.
  * This number defaults to 150 characters.
  *
  * @note calling `shorten` **will** mutate the Markdown AST node.
  */
class MarkdownShortener {
  import com.vladsch.flexmark.ast.{Text, Code, Image, FencedCodeBlock, BulletList, BulletListItem, OrderedListItem}
  import com.vladsch.flexmark.util.ast.{Node, NodeVisitor, VisitHandler, Visitor}

  def shorten(node: Node, maxLen: Int = 150): Node = {
    var len = 0
    var didUnlink = false

    def count(node: Node, length: => Int, shortenOrUnlink: Int => Unit) = {
      val remaining = math.max(maxLen - len, 0)
      if (didUnlink || remaining == 0) node.unlink()
      else {
        if (length <= remaining) len += length
        else {
          shortenOrUnlink(remaining)
          len = maxLen
        }
      }
    }

    val nodeVisitor = new NodeVisitor(
      new VisitHandler(classOf[Text], new Visitor[Text] {
        override def visit(node: Text) = count(
          node,
          node.getChars.length,
          remaining => node.setChars(
            node.getChars.subSequence(0, remaining).trimEnd.append("...")
          )
        )
      }),
      new VisitHandler(classOf[Code], new Visitor[Code] {
        override def visit(node: Code) = count(
          node,
          node.getText.length,
          remaining => node.setText(
            node.getText.subSequence(0, remaining).trimEnd.append("...")
          )
        )
      }),
      new VisitHandler(classOf[Image], new Visitor[Image] {
        override def visit(node: Image) = count(node, maxLen, _ => node.unlink())
      }),
      new VisitHandler(classOf[FencedCodeBlock], new Visitor[FencedCodeBlock] {
        override def visit(node: FencedCodeBlock) = count(node, maxLen, _ => node.unlink())
      }),
      new VisitHandler(classOf[BulletListItem], new Visitor[BulletListItem] {
        override def visit(node: BulletListItem) = count(
          node,
          if (didUnlink) maxLen
          else node.getSegments.map(_.length).reduceLeft(_ + _),
          _ => {
            node.unlink()
            didUnlink = true // unlink all following bullets
          }
        )
      }),
      new VisitHandler(classOf[OrderedListItem], new Visitor[OrderedListItem] {
        override def visit(node: OrderedListItem) = count(
          node,
          if (didUnlink) maxLen
          else node.getSegments.map(_.length).reduceLeft(_ + _),
          _ => {
            node.unlink()
            didUnlink = true // unlink all following bullets
          }
        )
      })
    )

    nodeVisitor.visit(node)
    node
  }
}
