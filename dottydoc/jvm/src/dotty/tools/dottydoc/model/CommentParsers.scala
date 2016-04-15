package dotty.tools
package dottydoc
package model

import dotc.core.Symbols.Symbol
import dotc.core.Contexts.Context

object CommentParsers {
  import comment._
  import BodyParsers._
  import Entities.{Entity, Package}

  sealed class WikiParser
  extends CommentCleaner with CommentParser with CommentExpander {
    def parseHtml(sym: Symbol, entity: Entity, packages: Map[String, Package])(implicit ctx: Context): Option[Comment] =
      ctx.base.docstring(sym).map { d =>
        val expanded = expand(sym)
        parse(entity, packages, clean(expanded), expanded, d.pos).toHtml(entity) match {
          case "" => None
          case x  => Some(Comment(x))
        }
      }.flatten
    }

  val wikiParser = new WikiParser
}
