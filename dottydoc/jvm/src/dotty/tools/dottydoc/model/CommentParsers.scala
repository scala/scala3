package dotty.tools
package dottydoc
package model

import dotc.core.Symbols.Symbol
import dotc.core.Contexts.Context

object CommentParsers {
  import comment._
  import BodyParsers._
  import model.internal._

  class WikiParser extends CommentCleaner with CommentParser with CommentExpander {
    private[this] var commentCache: Map[String, (Entity, Map[String, Package]) => Option[Comment]] = Map.empty

    /** Parses comment and returns the path to the entity with an optional comment
     *
     * The idea here is to use this fact to create `Future[Seq[(String, Option[Comment]]]`
     * which can then be awaited near the end of the run - before the pickling.
     */
    def parseHtml(sym: Symbol, entity: Entity, packages: Map[String, Package])(implicit ctx: Context): (String, Option[Comment]) = {
      val cmt = ctx.base.docstring(sym).map { d =>
        val expanded = expand(sym)
        val body = parse(entity, packages, clean(expanded), expanded, d.pos)
        val summary = body.summary.map(_.toHtml(entity)).getOrElse("")
         body.toHtml(entity) match {
          case "" => None
          case x  => Some(Comment(x, summary))
        }
      }.flatten

      (entity.path.mkString("."), cmt)
    }


    def add(entity: Entity, symbol: Symbol, ctx: Context): Unit = {
      val commentParser = { (entity: Entity, packs: Map[String, Package]) =>
        parseHtml(symbol, entity, packs)(ctx)._2
      }

      val path = entity.path.mkString(".")
      if (!commentCache.contains(path) || ctx.base.docstring(symbol).isDefined)
        commentCache = commentCache + (path -> commentParser)
    }

    def +=(entity: Entity, symbol: Symbol, ctx: Context) = add(entity, symbol, ctx)

    def parse(entity: Entity, packs: Map[String, Package]): Option[Comment] =
      commentCache(entity.path.mkString("."))(entity, packs)

    def clear(): Unit = commentCache = Map.empty

    def size: Int = commentCache.size
  }
}
