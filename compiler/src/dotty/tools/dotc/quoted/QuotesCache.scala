package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.ast.tpd


object QuotesCache {
  import tpd.*

  /** A cache from pickled TASTy payloads to unpickled trees.
   *  The value also records the owner of the first definition in the cached
   *  tree (`QuoteUtils.treeOwner`), if any, so that the hit path does not
   *  need to recompute it with a tree fold.
   */
  type QuoteCache = collection.mutable.Map[String | List[String], (Tree, Option[Symbol])]

  /** A key to be used in a context property that caches the unpickled trees */
  private val QuotesCacheKey = new Property.Key[QuoteCache]

  /** A fresh empty cache */
  def mkCache(): QuoteCache = collection.mutable.Map.empty

  /** Get the cached tree of the quote and the owner of its first definition (if any) */
  def getTree(pickled: String | List[String])(using Context): Option[(Tree, Option[Symbol])] =
    ctx.property(QuotesCacheKey).get.get(pickled)

  /** Update the cached tree of the quote */
  def update(pickled: String | List[String], tree: Tree, treeOwner: Option[Symbol])(using Context): Unit =
    ctx.property(QuotesCacheKey).get.update(pickled, (tree, treeOwner))

  /** Context with the given cache for quote trees and tasty bytes */
  def init(ctx: FreshContext, cache: QuoteCache = mkCache()): ctx.type =
    ctx.setProperty(QuotesCacheKey, cache)
}
