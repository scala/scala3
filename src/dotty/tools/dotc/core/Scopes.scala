/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.dotc
package core

import Symbols._
import Names._
import Periods._
import Decorators._
import Contexts._
import Denotations._
import SymDenotations.NoDenotation

object Scopes {

  /** Maximal fill factor of hash table */
  private final val FillFactor = 2.0/3.0

  /** A hashtable is created once current size exceeds MinHash * FillFactor
   *  The initial hash table has twice that size (i.e 24).
   */
  private final val MinHash = 12

  /** The maximal permissible number of recursions when creating
   *  a hashtable
   */
  private final val MaxRecursions = 1000

  class ScopeEntry private[Scopes] (val sym: Symbol, val owner: Scope) {

    /** the next entry in the hash bucket
     */
    var tail: ScopeEntry = null

    /** the preceding entry in this scope
     */
    var prev: ScopeEntry = null

    override def toString: String = sym.toString
  }

  /** Note: constructor is protected to force everyone to use the factory methods newScope or newNestedScope instead.
   *  This is necessary because when run from reflection every scope needs to have a
   *  SynchronizedScope as mixin.
   */
  class Scope protected[Scopes](initElems: ScopeEntry, initSize: Int, val nestingLevel: Int = 0)
      extends Iterable[Symbol] {

    protected[Scopes] def this(base: Scope)(implicit ctx: Context) = {
      this(base.lastEntry, base.size, base.nestingLevel + 1)
      ensureCapacity(MinHash)(ctx) // WTH??? it seems the implicit is not in scope for a secondary constructor call.
    }

    def this() = this(null, 0, 0)

    private[dotc] var lastEntry: ScopeEntry = initElems

    /** The size of the scope */
    private[this] var _size = initSize

    override def size = _size
    private def size_= (x: Int) = _size = x

    /** the hash table
     */
    private var hashTable: Array[ScopeEntry] = null

    /** a cache for all elements, to be used by symbol iterator.
     */
    private var elemsCache: List[Symbol] = null

    /** Returns a new scope with the same content as this one. */
    def cloneScope(implicit ctx: Context): Scope = newScopeWith(this.toList: _*)

    /** is the scope empty? */
    override def isEmpty: Boolean = lastEntry eq null

    /** create and enter a scope entry */
    protected def newScopeEntry(sym: Symbol)(implicit ctx: Context): ScopeEntry = {
      val e = new ScopeEntry(sym, this)
      e.prev = lastEntry
      lastEntry = e
      size += 1
      elemsCache = null
      if (hashTable ne null) {
        ensureCapacity(hashTable.length)
        enterInHash(e)
      } else {
        ensureCapacity(MinHash)
      }
      e
    }

    private def enterInHash(e: ScopeEntry)(implicit ctx: Context): Unit = {
      val i = e.sym.name.start & (hashTable.length - 1)
      e.tail = hashTable(i)
      hashTable(i) = e
    }

    /** enter a symbol
     *
     *  @param sym ...
     */
    def enter[T <: Symbol](sym: T)(implicit ctx: Context): T = {
      newScopeEntry(sym)
      sym
    }

    /** enter a symbol, asserting that no symbol with same name exists in scope
     *
     *  @param sym ...
     */
    def enterUnique(sym: Symbol)(implicit ctx: Context) {
      assert(lookup(sym.name) == NoSymbol, (sym.showLocated, lookup(sym.name).showLocated))
      enter(sym)
    }

    private def ensureCapacity(tableSize: Int)(implicit ctx: Context): Unit =
      if (size > tableSize * FillFactor) createHash(tableSize * 2)

    private def createHash(tableSize: Int)(implicit ctx: Context): Unit =
      if (size > tableSize * FillFactor) createHash(tableSize * 2)
      else {
        hashTable = new Array[ScopeEntry](tableSize)
        enterAllInHash(lastEntry)
      }

    private def enterAllInHash(e: ScopeEntry, n: Int = 0)(implicit ctx: Context) {
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

    /** remove entry from this scope. */
    def unlink(e: ScopeEntry)(implicit ctx: Context) {
      if (lastEntry == e) {
        lastEntry = e.prev
      } else {
        var e1 = lastEntry
        while (e1.prev != e) e1 = e1.prev
        e1.prev = e.prev
      }
      if (hashTable ne null) {
        val index = e.sym.name.start & (hashTable.length - 1)
        var e1 = hashTable(index)
        if (e1 == e)
          hashTable(index) = e.tail
        else {
          while (e1.tail != e) e1 = e1.tail;
          e1.tail = e.tail
        }
      }
      elemsCache = null
      size -= 1
    }

    /** remove symbol from this scope */
    def unlink(sym: Symbol)(implicit ctx: Context) {
      var e = lookupEntry(sym.name)
      while (e ne null) {
        if (e.sym == sym) unlink(e);
        e = lookupNextEntry(e)
      }
    }

    /** lookup a symbol
     *
     *  @param name ...
     *  @return     ...
     */
    def lookup(name: Name)(implicit ctx: Context): Symbol = {
      val e = lookupEntry(name)
      if (e eq null) NoSymbol else e.sym
    }

    /** Returns an iterator yielding every symbol with given name in this scope.
     */
    def lookupAll(name: Name)(implicit ctx: Context): Iterator[Symbol] = new Iterator[Symbol] {
      var e = lookupEntry(name)
      def hasNext: Boolean = e ne null
      def next(): Symbol = { val r = e.sym; e = lookupNextEntry(e); r }
    }

    /** The denotation set of all the symbols with given name in this scope */
    def denotsNamed(name: Name)(implicit ctx: Context): DenotationSet = {
      var syms: DenotationSet = NoDenotation
      var e = lookupEntry(name)
      while (e != null) {
        syms = syms union e.sym.denot
        e = lookupNextEntry(e)
      }
      syms
    }

    /** lookup a symbol entry matching given name.
     *  @note from Martin: I believe this is a hotspot or will be one
     *  in future versions of the type system. I have reverted the previous
     *  change to use iterators as too costly.
     */
    def lookupEntry(name: Name)(implicit ctx: Context): ScopeEntry = {
      var e: ScopeEntry = null
      if (hashTable ne null) {
        e = hashTable(name.start & (hashTable.length - 1))
        while ((e ne null) && e.sym.name != name) {
          e = e.tail
        }
      } else {
        e = lastEntry
        while ((e ne null) && e.sym.name != name) {
          e = e.prev
        }
      }
      e
    }

    /** lookup next entry with same name as this one */
    def lookupNextEntry(entry: ScopeEntry)(implicit ctx: Context): ScopeEntry = {
      var e = entry
      if (hashTable ne null)
        do { e = e.tail } while ((e ne null) && e.sym.name != entry.sym.name)
      else
        do { e = e.prev } while ((e ne null) && e.sym.name != entry.sym.name);
      e
    }

    /** Return all symbols as a list in the order they were entered in this scope.
     */
    override def toList: List[Symbol] = {
      if (elemsCache eq null) {
        elemsCache = Nil
        var e = lastEntry
        while ((e ne null) && e.owner == this) {
          elemsCache = e.sym :: elemsCache
          e = e.prev
        }
      }
      elemsCache
    }

    /** Vanilla scope - symbols are stored in declaration order.
     */
    def sorted: List[Symbol] = toList

    /** Return all symbols as an iterator in the order they were entered in this scope.
     */
    def iterator: Iterator[Symbol] = toList.iterator

    override def foreach[U](p: Symbol => U): Unit = toList foreach p

    def filteredScope(p: Symbol => Boolean)(implicit ctx: Context): Scope = {
      val unfiltered = toList
      val filtered = unfiltered filterConserve p
      if (filtered eq unfiltered) this
      else newScopeWith(filtered: _*)
    }

    @deprecated("Use `toList.reverse` instead", "2.10.0")
    def reverse: List[Symbol] = toList.reverse
  }

  /** Create a new scope */
  def newScope: Scope = new Scope()

  /** Create a new scope nested in another one with which it shares its elements */
  def newNestedScope(outer: Scope)(implicit ctx: Context): Scope = new Scope(outer)

  /** Create a new scope with given initial elements */
  def newScopeWith(elems: Symbol*)(implicit ctx: Context): Scope = {
    val scope = newScope
    elems foreach scope.enter
    scope
  }

  /** Create new scope for the members of package `pkg` */
  def newPackageScope(pkgClass: Symbol): Scope = newScope

  /** Transform scope of members of `owner` using operation `op`
   *  This is overridden by the reflective compiler to avoid creating new scopes for packages
   */
  def scopeTransform(owner: Symbol)(op: => Scope): Scope = op

  /** The empty scope (immutable).
   */
  object EmptyScope extends Scope {
    override def newScopeEntry(sym: Symbol)(implicit ctx: Context): ScopeEntry = {
      throw new AssertionError("EmptyScope.newScopeEntry")
    }
  }

  /** The error scope (mutable)
   */
  class ErrorScope(owner: Symbol) extends Scope
}
