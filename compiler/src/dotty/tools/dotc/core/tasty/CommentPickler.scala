package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Comments.{Comment, CommentsContext, ContextDocstrings}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.tasty.TastyBuffer.Addr

import java.nio.charset.Charset

class CommentPickler(pickler: TastyPickler, addrOfTree: tpd.Tree => Option[Addr])(implicit ctx: Context) {
  private[this] val buf = new TastyBuffer(5000)
  pickler.newSection("Comments", buf)

  def pickleComment(root: tpd.Tree): Unit = {
    assert(ctx.docCtx.isDefined, "Trying to pickle comments, but there's no `docCtx`.")
    new Traverser(ctx.docCtx.get).traverse(root)
  }

  def pickleComment(addrOfTree: Option[Addr], comment: Option[Comment]): Unit = (addrOfTree, comment) match {
    case (Some(addr), Some(cmt)) =>
      val bytes = cmt.raw.getBytes(Charset.forName("UTF-8"))
      val length = bytes.length
      buf.writeAddr(addr)
      buf.writeNat(length)
      buf.writeBytes(bytes, length)
      buf.writeLongInt(cmt.pos.coords)
    case other =>
      ()
  }

  private class Traverser(docCtx: ContextDocstrings) extends tpd.TreeTraverser {
    override def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit =
      tree match {
        case md: tpd.MemberDef =>
          val comment = docCtx.docstring(md.symbol)
          pickleComment(addrOfTree(md), comment)
          traverseChildren(md)
        case _ =>
          traverseChildren(tree)
      }
  }

}
