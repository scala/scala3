package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.{Comment, CommentsContext}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.tasty.TastyBuffer.Addr

import java.nio.charset.Charset

class CommentPickler(pickler: TastyPickler, addrOfTree: tpd.Tree => Option[Addr])(implicit ctx: Context) {
  val buf = new TastyBuffer(5000)
  pickler.newSection("Comments", buf)

  def pickleComment(root: tpd.Tree): Unit =
    new Traverser().traverse(root)

  def pickleComment(addrOfTree: Option[Addr], comment: Option[Comment]): Unit = (addrOfTree, comment) match {
    case (Some(addr), Some(cmt)) =>
      val bytes = cmt.raw.getBytes(Charset.forName("UTF-8"))
      val length = bytes.length
      buf.writeAddr(addr)
      buf.writeNat(length)
      buf.writeBytes(bytes, length)
      buf.writeBoolean(cmt.isExpanded)
    case other =>
      ()
  }

  private class Traverser extends tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit =
      tree match {
        case md: tpd.MemberDef =>
          ctx.docCtx.foreach { docCtx =>
            val comment = docCtx.docstring(md.symbol)
            pickleComment(addrOfTree(md), comment)
          }
          traverseChildren(md)
        case _ =>
          traverseChildren(tree)
      }
  }

}
