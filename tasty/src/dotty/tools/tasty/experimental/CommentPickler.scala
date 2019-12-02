package dotty.tools.tasty.experimental

import dotty.tools.tasty.TastyBuffer
import TastyBuffer.{Addr, NoAddr}

import java.nio.charset.Charset

class CommentPickler[T <: Tasty](val pickler: TastyPickler[T], addrOfTree: pickler.tasty.tpd.Tree => Addr) {
  import pickler.tasty.{_, given}
  private val buf = new TastyBuffer(5000)
  pickler.newSection("Comments", buf)

  def pickleComment(root: tpd.Tree)(implicit ctx: Context): Unit = {
    val docCtx = ctx.docCtx
    assert(docCtx.isDefined, "Trying to pickle comments, but there's no `docCtx`.")
    new Traverser(docCtx.get).traverse(root)
  }

  def pickleComment(addr: Addr, comment: Option[Comment]): Unit = comment match {
    case Some(cmt) if addr != NoAddr =>
      val bytes = cmt.raw.getBytes(Charset.forName("UTF-8"))
      val length = bytes.length
      buf.writeAddr(addr)
      buf.writeNat(length)
      buf.writeBytes(bytes, length)
      buf.writeLongInt(cmt.span.coords)
    case other =>
      ()
  }

  private class Traverser(docCtx: ContextDocstrings) extends TreeTraverser {
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
