package dotty.tools.dotc
package transform

import core._
import Flags._, Symbols._, Contexts._, Scopes._, Decorators._
import collection.mutable
import collection.immutable.BitSet
import scala.annotation.tailrec

/** A module that can produce a kind of iterator (`Cursor`),
 *  which yields all pairs of overriding/overridden symbols
 *  that are visible in some baseclass, unless there's a parent class
 *  that already contains the same pairs.
 *
 *  Adapted from the 2.9 version of OverridingPairs. The 2.10 version is IMO
 *  way too unwieldy to be maintained.
 */
object OverridingPairs {

  /** The cursor class
   *  @param base   the base class that contains the overriding pairs
   */
  class Cursor(base: Symbol)(using Context) {

    private val self = base.thisType

    /** Symbols to exclude: Here these are constructors and private locals.
     *  But it may be refined in subclasses.
     */
    protected def exclude(sym: Symbol): Boolean = !sym.memberCanMatchInheritedSymbols

    /** The parents of base that are checked when deciding whether an overriding
     *  pair has already been treated in a parent class.
     *  This may be refined in subclasses. @see Bridges for a use case.
     */
    protected def parents: Array[Symbol] = base.info.parents.toArray.map(_.typeSymbol)

    /** Does `sym1` match `sym2` so that it qualifies as overriding.
     *  Types always match. Term symbols match if their membertypes
     *  relative to <base>.this do
     */
    protected def matches(sym1: Symbol, sym2: Symbol): Boolean =
      sym1.isType || sym1.asSeenFrom(self).matches(sym2.asSeenFrom(self))

    /** The symbols that can take part in an overriding pair */
    private val decls = {
      val decls = newScope
      // fill `decls` with overriding shadowing overridden */
      def fillDecls(bcs: List[Symbol], deferred: Boolean): Unit = bcs match {
        case bc :: bcs1 =>
          fillDecls(bcs1, deferred)
          var e = bc.info.decls.lastEntry
          while (e != null) {
            if (e.sym.is(Deferred) == deferred && !exclude(e.sym))
              decls.enter(e.sym)
            e = e.prev
          }
        case nil =>
      }
      // first, deferred (this will need to change if we change lookup rules!
      fillDecls(base.info.baseClasses, deferred = true)
      // then, concrete.
      fillDecls(base.info.baseClasses, deferred = false)
      decls
    }

    /** Is `parent` a qualified sub-parent of `bc`?
     *  @pre `parent` is a parent class of `base` and it derives from `bc`.
     *  @return true if the `bc`-basetype of the parent's type is the same as
     *               the `bc`-basetype of base. In that case, overriding checks
     *               relative to `parent` already subsume overriding checks
     *               relative to `base`. See neg/11719a.scala for where this makes
     *               a difference.
     */
    protected def isSubParent(parent: Symbol, bc: Symbol)(using Context) =
      bc.typeParams.isEmpty
      || self.baseType(parent).baseType(bc) == self.baseType(bc)

    private val subParents = MutableSymbolMap[BitSet]()

    for bc <- base.info.baseClasses do
      var bits = BitSet.empty
      for i <- 0 until parents.length do
        if parents(i).derivesFrom(bc) && isSubParent(parents(i), bc)
        then bits += i
      subParents(bc) = bits

    private def hasCommonParentAsSubclass(cls1: Symbol, cls2: Symbol): Boolean =
      (subParents(cls1) intersect subParents(cls2)).nonEmpty

    /** The scope entries that have already been visited as overridden
     *  (maybe excluded because of hasCommonParentAsSubclass).
     *  These will not appear as overriding
     */
    private val visited = util.HashSet[Symbol]()

    /** The current entry candidate for overriding
     */
    private var curEntry = decls.lastEntry

    /** The current entry candidate for overridden */
    private var nextEntry = curEntry

    /** The current candidate symbol for overriding */
    var overriding: Symbol = _

    /** If not null: The symbol overridden by overriding */
    var overridden: Symbol = _

    //@M: note that next is called once during object initialization
    final def hasNext: Boolean = nextEntry ne null

    /**  @post
     *     curEntry   = the next candidate that may override something else
     *     nextEntry  = curEntry
     *     overriding = curEntry.sym
     */
    private def nextOverriding(): Unit = {
      @tailrec def loop(): Unit =
        if (curEntry ne null) {
          overriding = curEntry.sym
          if (visited.contains(overriding)) {
            curEntry = curEntry.prev
            loop()
          }
        }
      loop()
      nextEntry = curEntry
    }

    /** @post
     *    hasNext    = there is another overriding pair
     *    overriding = overriding member of the pair, provided hasNext is true
     *    overridden = overridden member of the pair, provided hasNext is true
     */
    @tailrec final def next(): Unit =
      if (nextEntry ne null) {
        nextEntry = decls.lookupNextEntry(nextEntry)
        if (nextEntry ne null)
          try {
            overridden = nextEntry.sym
            if (overriding.owner != overridden.owner && matches(overriding, overridden)) {
              visited += overridden
              if (!hasCommonParentAsSubclass(overriding.owner, overridden.owner)) return
            }
          }
          catch {
            case ex: TypeError =>
              // See neg/i1750a for an example where a cyclic error can arise.
              // The root cause in this example is an illegal "override" of an inner trait
              report.error(ex, base.srcPos)
          }
        else {
          curEntry = curEntry.prev
          nextOverriding()
        }
        next()
      }

    nextOverriding()
    next()
  }
}
