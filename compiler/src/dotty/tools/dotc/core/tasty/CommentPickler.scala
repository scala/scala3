package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Comments.Comment

import dotty.tools.tasty.TastyBuffer
import TastyBuffer.{Addr, NoAddr}
import dotty.tools.tasty.TastyFormat.CommentsSection

import java.nio.charset.StandardCharsets

object CommentPickler:

  def pickleComments(
    pickler: TastyPickler,
    addrOfTree: PositionPickler.TreeToAddr,
    docString: untpd.MemberDef => Option[Comment],
    root: tpd.Tree,
    buf: TastyBuffer = new TastyBuffer(5000)): Unit =

    pickler.newSection(CommentsSection, buf)

    def pickleComment(addr: Addr, comment: Comment): Unit =
      if addr != NoAddr then
        val bytes = comment.raw.getBytes(StandardCharsets.UTF_8).nn
        val length = bytes.length
        buf.writeAddr(addr)
        buf.writeNat(length)
        buf.writeBytes(bytes, length)
        buf.writeLongInt(comment.span.coords)

    def traverse(x: Any): Unit = x match
      case x: untpd.Tree @unchecked =>
        x match
          case x: tpd.MemberDef @unchecked => // at this point all MemberDefs are t(y)p(e)d.
            for comment <- docString(x) do pickleComment(addrOfTree(x), comment)
          case _ =>
        val limit = x.productArity
        var n = 0
        while n < limit do
          traverse(x.productElement(n))
          n += 1
      case y :: ys =>
        traverse(y)
        traverse(ys)
      case _ =>

    traverse(root)
  end pickleComments
end CommentPickler

