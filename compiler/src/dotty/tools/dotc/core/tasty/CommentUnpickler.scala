package dotty.tools.dotc
package core.tasty

import core.Comments.Comment
import util.Spans.Span
import scala.collection.mutable

import dotty.tools.tasty.{TastyReader, TastyBuffer}
import TastyBuffer.Addr

import java.nio.charset.StandardCharsets

class CommentUnpickler(reader: TastyReader) {
  import reader.*

  private[tasty] lazy val comments: mutable.HashMap[Addr, Comment] = {
    val comments = mutable.HashMap.empty[Addr, Comment]
    while (!isAtEnd) {
      val addr = readAddr()
      val rawComment = readUtf8()
      val position = new Span(readLongInt())
      comments(addr) = Comment(position, rawComment)
    }
    comments
  }

  def commentAt(addr: Addr): Option[Comment] =
    comments.get(addr)
}
