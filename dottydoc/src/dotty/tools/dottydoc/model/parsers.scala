package dotty.tools
package dottydoc
package model

import dotc.core.Symbols.Symbol
import dotc.core.Contexts.Context
import dotc.util.Positions.NoPosition

object parsers {
  import comment._
  import BodyParsers._
  import model.internal._
  import util.MemberLookup
  import util.traversing._
  import util.internal.setters._

  class WikiParser extends CommentCleaner with CommentParser with CommentExpander {
    private[this] var commentCache: Map[String, (Entity, Map[String, Package]) => Option[Comment]] = Map.empty

    /** Parses comment and returns the path to the entity with an optional comment
     *
     * The idea here is to use this fact to create `Future[Seq[(String, Option[Comment]]]`
     * which can then be awaited near the end of the run - before the pickling.
     */
    def parseHtml(sym: Symbol, parent: Symbol, entity: Entity, packages: Map[String, Package])(implicit ctx: Context): (String, Option[Comment]) = {
      val cmt = ctx.docbase.docstring(sym).map { d =>
        val expanded = expand(sym, parent)
        parse(entity, packages, clean(expanded), expanded, d.pos).toComment(_.toHtml(entity))
      }

      (entity.path.mkString("."), cmt)
    }


    def add(entity: Entity, symbol: Symbol, parent: Symbol, ctx: Context): Unit = {
      val commentParser = { (entity: Entity, packs: Map[String, Package]) =>
        parseHtml(symbol, parent, entity, packs)(ctx)._2
      }

      /** TODO: this if statement searches for doc comments in parent
       *  definitions if one is not defined for the current symbol.
       *
       *  It might be a good idea to factor this out of the WikiParser - since
       *  it mutates the state of docbase sort of silently.
       */
      implicit val implCtx = ctx
      if (!ctx.docbase.docstring(symbol).isDefined) {
        val parentCmt =
          symbol.extendedOverriddenSymbols
          .find(ctx.docbase.docstring(_).isDefined)
          .flatMap(p => ctx.docbase.docstring(p))

        ctx.docbase.addDocstring(symbol, parentCmt)
      }


      val path = entity.path.mkString(".")
      if (!commentCache.contains(path) || ctx.docbase.docstring(symbol).isDefined)
        commentCache = commentCache + (path -> commentParser)
    }

    def +=(entity: Entity, symbol: Symbol, parent: Symbol, ctx: Context) = add(entity, symbol, parent, ctx)

    def size: Int = commentCache.size

    private def parse(entity: Entity, packs: Map[String, Package]): Option[Comment] =
      commentCache(entity.path.mkString("."))(entity, packs)

    def parse(packs: Map[String, Package]): Unit = {
      def rootPackages: List[String] = {
        var currentDepth = Int.MaxValue
        var packages: List[String] = Nil

        for (key <- packs.keys) {
          val keyDepth = key.split("\\.").length
          packages =
            if (keyDepth < currentDepth) {
              currentDepth = keyDepth
              key :: Nil
            } else if (keyDepth == currentDepth) {
              key :: packages
            } else packages
        }

        packages
      }

      for (pack <- rootPackages) {
        mutateEntities(packs(pack)) { e =>
          val comment = parse(e, packs)
          setComment(e, to = comment)
        }
      }
    }

    def clear(): Unit = commentCache = Map.empty
  }
}
