package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.ast.tpd


object QuotesCache {
  import tpd._

  /** A key to be used in a context property that caches the unpickled trees */
  private val QuotesCacheKey = new Property.Key[collection.mutable.Map[String | List[String], Tree]]


  /** Get the cached tree of the quote */
  def getTree(pickled: String | List[String])(using Context): Option[Tree] =
    ctx.property(QuotesCacheKey).get.get(pickled)

  /** Update the cached tree of the quote */
  def update(pickled: String | List[String], tree: Tree)(using Context): Unit =
    ctx.property(QuotesCacheKey).get.update(pickled, tree)

  /** Context with a cache for quote trees and tasty bytes */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(QuotesCacheKey, collection.mutable.Map.empty)
}
