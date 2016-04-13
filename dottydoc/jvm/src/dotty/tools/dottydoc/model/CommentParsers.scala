package dotty.tools
package dottydoc
package model

import dotc.core.Symbols.Symbol
import dotc.core.Contexts.Context

object CommentParsers {
  import comment._
  import BodyParsers._

  sealed class WikiParser
  extends CommentCleaner with CommentParser with CommentExpander {
    def parseHtml(sym: Symbol)(implicit ctx: Context): Option[Comment]= {
      println("Original ---------------------")
      println(ctx.base.docstring(sym).map(_.chrs).getOrElse(""))
      val expanded = expand(sym)
      println("Expanded ---------------------")
      println(expanded)
      parse(clean(expanded), expanded).toHtml match {
        case "" => None
        case x  => Some(Comment(x))
      }
    }
  }

  val wikiParser = new WikiParser
}
