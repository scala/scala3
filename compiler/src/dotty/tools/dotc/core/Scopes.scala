/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package dotc
package core

import Symbols._
import Types.{TermRef, NoPrefix}
import Flags._
import Names._
import Contexts._
import Denotations._
import SymDenotations._
import printing.Texts._
import printing.Printer
import util.common._
import SymDenotations.NoDenotation
import collection.mutable

object Scopes {

  /** Maximal fill factor of hash table */
  private final val FillFactor = 2.0/3.0

  /** A hashtable is created once current size exceeds MinHash * FillFactor
   *  The initial hash table has twice that size (i.e 16).
   *  This value must be a power of two, so that the index of an element can
   *  be computed as element.hashCode & (hashTable.length - 1)
   */
  final val MinHashedScopeSize = 8

  /** The maximal permissible number of recursions when creating
   *  a hashtable
   */
  private final val MaxRecursions = 1000

  /** A function that optionally produces synthesized symbols with
   *  the given name in the given context. Returns `NoSymbol` if the
   *  no symbol should be synthesized for the given name.
   */
  type SymbolSynthesizer = Name => Context => Symbol

  class ScopeEntry private[Scopes] (val name: Name, _sym: Symbol, val owner: Scope) {

    var sym: Symbol = _sym

    /** the next entry in the hash bucket
     */
    var tail: ScopeEntry = null

    /** the preceding entry in this scope
     */
    var prev: ScopeEntry = null

    override def toString: String = sym.toString
  }

  /** A scope contains a set of symbols. It can be an extension
   *  of some outer scope, from which it inherits all symbols.
   *  This class does not have any methods to add symbols to a scope
   *  or to delete them. These methods are provided by subclass
   *  MutableScope.
   */
  abstract class Scope extends printing.Showable {

    /** The last scope-entry from which all others are reachable via `prev` */
    private[dotc] def lastEntry: ScopeEntry

    /** The number of symbols in this scope (including inherited ones
     *  from outer scopes).
     */
    def size: Int

    /** The number of outer scopes from which symbols are inherited */
    def nestingLevel: Int

    /** The symbols in this scope in the order they were entered;
     *  inherited from outer ones first.
     */
    def toList(implicit ctx: Context): List[Symbol]

    /** Return all symbols as an iterator in the order they were entered in this scope.
     */
    def iterator(implicit ctx: Context): Iterator[Symbol] = toList.iterator

    /** Is the scope empty? */
    def isEmpty: Boolean = lastEntry eq null

    /** Applies a function f to all Symbols of this Scope. */
    def foreach[U](f: Symbol => U)(implicit ctx: Context): Unit = toList.foreach(f)

    /** Selects all Symbols of this Scope which satisfy a predicate. */
    def filter(p: Symbol => Boolean)(implicit ctx: Context): List[Symbol] = {
      ensureComplete()
      var syms: List[Symbol] = Nil
      var e = lastEntry
      while ((e ne null) && e.owner == this) {
        val sym = e.sym
        if (p(sym)) syms = sym :: syms
        e = e.prev
      }
      syms
    }

    /** Tests whether a predicate holds for at least one Symbol of this Scope. */
    def exists(p: Symbol => Boolean)(implicit ctx: Context): Boolean = filter(p).nonEmpty

    /** Finds the first Symbol of this Scope satisfying a predicate, if any. */
    def find(p: Symbol => Boolean)(implicit ctx: Context): Symbol = filter(p) match {
      case sym :: _ => sym
      case _ => NoSymbol
    }

    /** Returns a new mutable scope with the same content as this one. */
    def cloneScope(implicit ctx: Context): MutableScope

    /** Lookup a symbol entry matching given name. */
    def lookupEntry(name: Name)(implicit ctx: Context): ScopeEntry

    /** Lookup next entry with same name as this one */
    def lookupNextEntry(entry: ScopeEntry)(implicit ctx: Context): ScopeEntry

    /** Lookup a symbol */
    final def lookup(name: Name)(implicit ctx: Context): Symbol = {
      val e = lookupEntry(name)
      if (e eq null) NoSymbol else e.sym
    }

    /** Returns an iterator yielding every symbol with given name in this scope.
     */
    final def lookupAll(name: Name)(implicit ctx: Context): Iterator[Symbol] = new Iterator[Symbol] {
      var e = lookupEntry(name)
      def hasNext: Boolean = e ne null
      def next(): Symbol = { val r = e.sym; e = lookupNextEntry(e); r }
    }

    /** The denotation set of all the symbols with given name in this scope
     *  Symbols occur in the result in reverse order relative to their occurrence
     *  in `this.toList`.
     */
    final def denotsNamed(name: Name, select: SymDenotation => Boolean = selectAll)(implicit ctx: Context): PreDenotation = {
      var syms: PreDenotation = NoDenotation
      var e = lookupEntry(name)
      while (e != null) {
        val d = e.sym.denot
        if (select(d)) syms = syms union d
        e = lookupNextEntry(e)
      }
      syms
    }

    /** The scope that keeps only those symbols from this scope that match the
     *  given predicates. If all symbols match, returns the scope itself, otherwise
     *  a copy with the matching symbols.
     */
    final def filteredScope(p: Symbol => Boolean)(implicit ctx: Context): Scope = {
      var result: MutableScope = null
      for (sym <- iterator)
        if (!p(sym)) {
          if (result == null) result = cloneScope
          result.unlink(sym)
        }
      if (result == null) this else result
    }

    def implicitDecls(implicit ctx: Context): List[TermRef] = Nil

    def openForMutations: MutableScope = unsupported("openForMutations")

    final def toText(printer: Printer): Text = printer.toText(this)

    def checkConsistent()(implicit ctx: Context): Unit = ()

    /** Ensure that all elements of this scope have been entered.
     *  Overridden by SymbolLoaders.PackageLoader#PackageScope, where it
     *  makes sure that all names with `$`'s have been added.
     */
    protected def ensureComplete()(implicit ctx: Context): Unit = ()
  }

  /** A subclass of Scope that defines methods for entering and
   *  unlinking entries.
   *  Note: constructor is protected to force everyone to use the factory methods newScope or newNestedScope instead.
   *  This is necessary because when run from reflection every scope needs to have a
   *  SynchronizedScope as mixin.
   */
  class MutableScope protected[Scopes](initElems: ScopeEntry, initSize: Int, val nestingLevel: Int = 0)
      extends Scope {

    /** Scope shares elements with `base` */
    protected[Scopes] def this(base: Scope)(implicit ctx: Context) = {
      this(base.lastEntry, base.size, base.nestingLevel + 1)
      ensureCapacity(MinHashedScopeSize)(ctx) // WTH? it seems the implicit is not in scope for a secondary constructor call.
    }

    def this() = this(null, 0, 0)

    private[dotc] var lastEntry: ScopeEntry = initElems

    /** The size of the scope */
    private[this] var _size = initSize

    override final def size: Int = _size
    private def size_= (x: Int) = _size = x

    /** the hash table
     */
    private[this] var hashTable: Array[ScopeEntry] = null

    /** a cache for all elements, to be used by symbol iterator.
     */
    private[this] var elemsCache: List[Symbol] = null

    /** The synthesizer to be used, or `null` if no synthesis is done on this scope */
    private var synthesize: SymbolSynthesizer = null

    /** Use specified synthesize for this scope */
    def useSynthesizer(s: SymbolSynthesizer): Unit = synthesize = s

    protected def newScopeLikeThis(): MutableScope = new MutableScope()

    /** Clone scope, taking care not to force the denotations of any symbols in the scope.
     */
    def cloneScope(implicit ctx: Context): MutableScope = {
      val entries = new mutable.ArrayBuffer[ScopeEntry]
      var e = lastEntry
      while ((e ne null) && e.owner == this) {
        entries += e
        e = e.prev
      }
      val scope = newScopeLikeThis()
      for (i <- entries.length - 1 to 0 by -1) {
        val e = entries(i)
        scope.newScopeEntry(e.name, e.sym)
      }
      scope.synthesize = synthesize
      scope
    }

    /** create and enter a scope entry with given name and symbol */
    protected def newScopeEntry(name: Name, sym: Symbol)(implicit ctx: Context): ScopeEntry = {
      ensureCapacity(if (hashTable ne null) hashTable.length else MinHashedScopeSize)
      val e = new ScopeEntry(name, sym, this)
      e.prev = lastEntry
      lastEntry = e
      if (hashTable ne null) enterInHash(e)
      size += 1
      elemsCache = null
      e
    }

    /** create and enter a scope entry */
    protected def newScopeEntry(sym: Symbol)(implicit ctx: Context): ScopeEntry =
      newScopeEntry(sym.name, sym)

    private def enterInHash(e: ScopeEntry)(implicit ctx: Context): Unit = {
      val idx = e.name.hashCode & (hashTable.length - 1)
      e.tail = hashTable(idx)
      assert(e.tail != e)
      hashTable(idx) = e
    }

    /** enter a symbol in this scope. */
    final def enter[T <: Symbol](sym: T)(implicit ctx: Context): T = {
      if (sym.isType && ctx.phaseId <= ctx.typerPhase.id) {
        assert(lookup(sym.name) == NoSymbol,
          s"duplicate ${sym.debugString}; previous was ${lookup(sym.name).debugString}") // !!! DEBUG
      }
      newScopeEntry(sym)
      sym
    }

    /** enter a symbol, asserting that no symbol with same name exists in scope */
    final def enterUnique(sym: Symbol)(implicit ctx: Context): Unit = {
      assert(lookup(sym.name) == NoSymbol, (sym.showLocated, lookup(sym.name).showLocated))
      enter(sym)
    }

    private def ensureCapacity(tableSize: Int)(implicit ctx: Context): Unit =
      if (size >= tableSize * FillFactor) createHash(tableSize * 2)

    private def createHash(tableSize: Int)(implicit ctx: Context): Unit =
      if (size > tableSize * FillFactor) createHash(tableSize * 2)
      else {
        hashTable = new Array[ScopeEntry](tableSize)
        enterAllInHash(lastEntry)
        // checkConsistent() // DEBUG
      }

    private def enterAllInHash(e: ScopeEntry, n: Int = 0)(implicit ctx: Context): Unit = {
      if (e ne null) {
        if (n < MaxRecursions) {
          enterAllInHash(e.prev, n + 1)
          enterInHash(e)
        } else {
          var entries: List[ScopeEntry] = List()
          var ee = e
          while (ee ne null) {
            entries = ee :: entries
            ee = ee.prev
          }
          entries foreach enterInHash
        }
      }
    }

    /** Remove entry from this scope (which is required to be present) */
    final def unlink(e: ScopeEntry)(implicit ctx: Context): Unit = {
      if (lastEntry == e) {
        lastEntry = e.prev
      } else {
        var e1 = lastEntry
        while (e1.prev != e) e1 = e1.prev
        e1.prev = e.prev
      }
      if (hashTable ne null) {
        val index = e.name.hashCode & (hashTable.length - 1)
        var e1 = hashTable(index)
        if (e1 == e)
          hashTable(index) = e.tail
        else {
          while (e1.tail != e) e1 = e1.tail
          e1.tail = e.tail
        }
      }
      elemsCache = null
      size -= 1
    }

    /** remove symbol from this scope if it is present */
    final def unlink(sym: Symbol)(implicit ctx: Context): Unit =
      unlink(sym, sym.name)

    /** remove symbol from this scope if it is present under the given name */
    final def unlink(sym: Symbol, name: Name)(implicit ctx: Context): Unit = {
      var e = lookupEntry(name)
      while (e ne null) {
        if (e.sym == sym) unlink(e)
        e = lookupNextEntry(e)
      }
    }

    /** Replace symbol `prev` (if it exists in current scope) by symbol `replacement`.
     *  @pre `prev` and `replacement` have the same name.
     */
    final def replace(prev: Symbol, replacement: Symbol)(implicit ctx: Context): Unit = {
      require(prev.name == replacement.name)
      var e = lookupEntry(prev.name)
      while (e ne null) {
        if (e.sym == prev) e.sym = replacement
        e = lookupNextEntry(e)
      }
      elemsCache = null
    }

    /** Lookup a symbol entry matching given name.
     */
    override def lookupEntry(name: Name)(implicit ctx: Context): ScopeEntry = {
      var e: ScopeEntry = null
      if (hashTable ne null) {
        e = hashTable(name.hashCode & (hashTable.length - 1))
        while ((e ne null) && e.name != name) {
          e = e.tail
        }
      } else {
        e = lastEntry
        while ((e ne null) && e.name != name) {
          e = e.prev
        }
      }
      if ((e eq null) && (synthesize != null)) {
        val sym = synthesize(name)(ctx)
        if (sym.exists) newScopeEntry(sym) else e
      }
      else e
    }

    /** lookup next entry with same name as this one */
    override final def lookupNextEntry(entry: ScopeEntry)(implicit ctx: Context): ScopeEntry = {
      var e = entry
      if (hashTable ne null)
        do { e = e.tail } while ((e ne null) && e.name != entry.name)
      else
        do { e = e.prev } while ((e ne null) && e.name != entry.name)
      e
    }

    /** Returns all symbols as a list in the order they were entered in this scope.
     *  Does _not_ include the elements of inherited scopes.
     */
    override final def toList(implicit ctx: Context): List[Symbol] = {
      if (elemsCache eq null) {
        ensureComplete()
        elemsCache = Nil
        var e = lastEntry
        while ((e ne null) && e.owner == this) {
          elemsCache = e.sym :: elemsCache
          e = e.prev
        }
      }
      elemsCache
    }

    override def implicitDecls(implicit ctx: Context): List[TermRef] = {
      ensureComplete()
      var irefs = new mutable.ListBuffer[TermRef]
      var e = lastEntry
      while (e ne null) {
        if (e.sym is ImplicitOrImpliedOrGiven) {
          val d = e.sym.denot
          irefs += TermRef(NoPrefix, d.symbol.asTerm).withDenot(d)
        }
        e = e.prev
      }
      irefs.toList
    }

    /** Vanilla scope - symbols are stored in declaration order.
     */
    final def sorted(implicit ctx: Context): List[Symbol] = toList

    override def openForMutations: MutableScope = this

    /** Check that all symbols in this scope are in their correct hashtable buckets. */
    override def checkConsistent()(implicit ctx: Context): Unit = {
      ensureComplete()
      var e = lastEntry
      while (e != null) {
        var e1 = lookupEntry(e.name)
        while (e1 != e && e1 != null) e1 = lookupNextEntry(e1)
        assert(e1 == e, s"PANIC: Entry ${e.name} is badly linked")
        e = e.prev
      }
    }
  }

  /** Create a new scope */
  def newScope: MutableScope = new MutableScope()

  /** Create a new scope nested in another one with which it shares its elements */
  def newNestedScope(outer: Scope)(implicit ctx: Context): MutableScope = new MutableScope(outer)

  /** Create a new scope with given initial elements */
  def newScopeWith(elems: Symbol*)(implicit ctx: Context): MutableScope = {
    val scope = newScope
    elems foreach scope.enter
    scope
  }

  /** Transform scope of members of `owner` using operation `op`
   *  This is overridden by the reflective compiler to avoid creating new scopes for packages
   */
  def scopeTransform(owner: Symbol)(op: => MutableScope): MutableScope = op

  val selectAll: SymDenotation => Boolean = alwaysTrue
  val selectPrivate: SymDenotation => Boolean    = d => (d.flagsUNSAFE is Flags.Private)
  val selectNonPrivate: SymDenotation => Boolean = d => !(d.flagsUNSAFE is Flags.Private)

  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    override private[dotc] def lastEntry: ScopeEntry = null
    override def size: Int = 0
    override def nestingLevel: Int = 0
    override def toList(implicit ctx: Context): List[Symbol] = Nil
    override def cloneScope(implicit ctx: Context): MutableScope = unsupported("cloneScope")
    override def lookupEntry(name: Name)(implicit ctx: Context): ScopeEntry = null
    override def lookupNextEntry(entry: ScopeEntry)(implicit ctx: Context): ScopeEntry = null
  }

  /** A class for error scopes (mutable)
   */
  class ErrorScope(owner: Symbol) extends MutableScope
}
