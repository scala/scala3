package dotty.tools.dotc
package core.tasty

import scala.language.unsafeNulls

import core.Comments.Comment
import util.Spans.Span
import util.HashMap

import dotty.tools.tasty.{TastyReader, TastyBuffer}
import TastyBuffer.Addr

import java.nio.charset.StandardCharsets

class CommentUnpickler(reader: TastyReader) {
  import reader._

  private[tasty] lazy val comments: HashMap[Addr, Comment] = {
    val comments = new HashMap[Addr, Comment]
    while (!isAtEnd) {
      val addr = readAddr()
      val length = readNat()
      if (length > 0) {
        val bytes = readBytes(length)
        val position = new Span(readLongInt())
        val rawComment = new String(bytes, StandardCharsets.UTF_8)
        comments(addr) = Comment(position, rawComment)
      }
    }
    comments
  }

  def commentAt(addr: Addr): Option[Comment] =
    comments.get(addr)
}
