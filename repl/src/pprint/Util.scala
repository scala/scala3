package pprint

import scala.annotation.{switch, tailrec}

object Util{


  def concat[T](is: (() => Iterator[T])*) = new ConcatIterator(is.iterator.map(_()), () => Iterator.empty)
  /**
    * Basically a fast, efficient `.flatten` or `mkString` for nested iterators
    *
    * For some reason, the default way of concatenation e.g.
    *
    * val middle = first ++ lastChildIter ++ sep ++ remaining
    *
    * Was throwing weird NullPointerExceptions I couldn't figure out =(
    *
    * Also, ++ didn't seem to be sufficiently lazy, so it was forcing
    * things earlier than it really needed to. It isn't documented anywhere
    * how lazy it's meant to be, whereas `ConcatIterator` here is obviously
    * lazy and won't even evaluate each iterator until you ask it to
    */
  class ConcatIterator[T](it0: Iterator[Iterator[T]], joiner: () => Iterator[T]) extends Iterator[T]{
    var head: Iterator[T] = null.asInstanceOf[Iterator[T]]
    var count = 0

    @tailrec private def check(): Boolean = {

      if (head != null && head.hasNext) true
      else if (!it0.hasNext) false
      else {
        if (count % 2 == 0) head = it0.next()
        else head = joiner()
        count += 1

        check()
      }
    }

    def hasNext = check()


    def next() = {
      check()
      head.next()
    }
  }

  def isOperator(ident: String) = {
    (ident.size > 0) && (ident(0) match{
      case '<' | '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | /*'<' | */
           '>' | '?' | ':' | '=' | '&' |
           '|' | '/' | '\\' => true
      case _ => false
    })
  }
  def escapeChar(c: Char,
                 sb: StringBuilder,
                 unicode: Boolean = true) = (c: @switch) match {
    case '"' => sb.append("\\\"")
    case '\\' => sb.append("\\\\")
    case '\b' => sb.append("\\b")
    case '\f' => sb.append("\\f")
    case '\n' => sb.append("\\n")
    case '\r' => sb.append("\\r")
    case '\t' => sb.append("\\t")
    case c =>
      if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
      else sb.append(c)
  }


  /**
    * Convert a string to a C&P-able literal. Basically
    * copied verbatim from the uPickle source code.
    */
  def literalize(s: IndexedSeq[Char], unicode: Boolean = true) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      Util.escapeChar(s(i), sb, unicode)
      i += 1
    }
    sb.append('"')

    sb.result()
  }
}
