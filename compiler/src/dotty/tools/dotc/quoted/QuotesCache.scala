package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.util.Property
import dotty.tools.dotc.ast.tpd


object QuotesCache {
  import tpd.*

  /** Only used when the cached tree includes symbol definition. 
   * Represents a mapping from the symbol owning the context of the quote to the unpickled tree. */
  private type OwnerCache = collection.mutable.Map[Symbol, Tree]

  /** A key to be used in a context property that caches the unpickled trees */
  private val QuotesCacheKey = new Property.Key[collection.mutable.Map[String | List[String], Either[Tree, OwnerCache]]]


  /** Get the cached tree of the quote. 
   *  quoteOwner is taken into account only if the unpickled quote includes a symbol definition */
  def getTree(pickled: String | List[String], quoteOwner: Symbol)(using Context): Option[Tree] =
    ctx.property(QuotesCacheKey).get.get(pickled).flatMap {
      case Left(tree: Tree) => Some(tree)
      case Right(map) => map.get(quoteOwner)
    }

  /** Update the cached tree of the quote.
   *  quoteOwner is applicable only if the quote includes a symbol definition, otherwise should be None */
  def update(pickled: String | List[String], quoteOwner: Option[Symbol], tree: Tree)(using Context): Unit =
    val previousValueMaybe = ctx.property(QuotesCacheKey).get.get(pickled)
    val updatedValue: Either[Tree, OwnerCache] =
      (previousValueMaybe, quoteOwner) match
        case (None, Some(owner)) =>
          Right(collection.mutable.Map((owner, tree)))
        case (Some(map: OwnerCache), Some(owner)) =>
          map.update(owner, tree)
          Right(map)
        case _ => Left(tree)
    ctx.property(QuotesCacheKey).get.update(pickled, updatedValue)

  /** Context with a cache for quote trees and tasty bytes */
  def init(ctx: FreshContext): ctx.type =
    ctx.setProperty(QuotesCacheKey, collection.mutable.Map.empty)
}
