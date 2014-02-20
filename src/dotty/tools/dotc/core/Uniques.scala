package dotty.tools.dotc
package core

import Types._, Contexts._, util.Stats._, Hashable._, Names._
import util.HashSet

/** Defines operation `unique` for hash-consing types.
 *  Also defines specialized hash sets for hash consing uniques of a specific type.
 *  All sets offer a `enterIfNew` method which checks whether a type
 *  with the given parts exists already and creates a new one if not.
 */
object Uniques {

  private def recordCaching(tp: Type): Unit = recordCaching(tp.hash, tp.getClass)
  private def recordCaching(h: Int, clazz: Class[_]): Unit =
    if (h == NotCached) {
      record("uncached-types")
      record(s"uncached: $clazz")
    } else {
      record("cached-types")
      record(s"cached: $clazz")
    }

  def unique[T <: Type](tp: T)(implicit ctx: Context): T = {
    if (monitored) recordCaching(tp)
    if (tp.hash == NotCached) tp
    else if (monitored) {
      val size = ctx.uniques.size
      val result = ctx.uniques.findEntryOrUpdate(tp).asInstanceOf[T]
      if (ctx.uniques.size > size) record(s"fresh unique ${tp.getClass}")
      result
    } else ctx.uniques.findEntryOrUpdate(tp).asInstanceOf[T]
  } /* !!! DEBUG
  ensuring (
    result => tp.toString == result.toString || {
      println(s"cache mismatch; tp = $tp, cached = $result")
      false
    }
  )
 */

  final class NamedTypeUniques extends HashSet[NamedType]("uniqueNamedTypes", initialUniquesCapacity) with Hashable {
    override def hash(x: NamedType): Int = x.hash

    private def findPrevious(h: Int, prefix: Type, name: Name): NamedType = {
      var e = findEntryByHash(h)
      while (e != null) {
        if ((e.prefix == prefix) && (e.name eq name)) return e
        e = nextEntryByHash(h)
      }
      e
    }

    def enterIfNew(prefix: Type, name: Name): NamedType = {
      val h = doHash(name, prefix)
      if (monitored) recordCaching(h, classOf[CachedTermRef])
      def newType =
        if (name.isTypeName) new CachedTypeRef(prefix, name.asTypeName, h)
        else new CachedTermRef(prefix, name.asTermName, h)
      if (h == NotCached) newType
      else {
        val r = findPrevious(h, prefix, name)
        if (r ne null) r else addEntryAfterScan(newType)
      }
    }
  }

  final class TypeBoundsUniques extends HashSet[TypeBounds]("uniqueTypeBounds", initialUniquesCapacity) with Hashable {
    override def hash(x: TypeBounds): Int = x.hash

    private def findPrevious(h: Int, lo: Type, hi: Type, variance: Int): TypeBounds = {
      var e = findEntryByHash(h)
      while (e != null) {
        if ((e.lo == lo) && (e.hi == hi) && (e.variance == variance)) return e
        e = nextEntryByHash(h)
      }
      e
    }

    def enterIfNew(lo: Type, hi: Type, variance: Int): TypeBounds = {
      val h = doHash(variance, lo, hi)
      if (monitored) recordCaching(h, classOf[TypeBounds])
      def newBounds =
        if (variance == 0) new CachedTypeBounds(lo, hi, h)
        else if (variance == 1) new CoTypeBounds(lo, hi, h)
        else new ContraTypeBounds(lo, hi, h)
      if (h == NotCached) newBounds
      else {
        val r = findPrevious(h, lo, hi, variance)
        if (r ne null) r else addEntryAfterScan(newBounds)
      }
    }
  }

  final class RefinedUniques extends HashSet[RefinedType]("uniqueRefinedTypes", initialUniquesCapacity) with Hashable {
    override val hashSeed = classOf[CachedRefinedType].hashCode // some types start life as CachedRefinedTypes, need to have same hash seed
    override def hash(x: RefinedType): Int = x.hash

    private def findPrevious(h: Int, parent: Type, refinedName: Name, refinedInfo: Type): RefinedType = {
      var e = findEntryByHash(h)
      while (e != null) {
        if ((e.parent == parent) && (e.refinedName eq refinedName) && (e.refinedInfo == refinedInfo))
          return e
        e = nextEntryByHash(h)
      }
      e
    }

    def enterIfNew(parent: Type, refinedName: Name, refinedInfo: Type): RefinedType = {
      val h = doHash(refinedName, refinedInfo, parent)
      def newType = new PreHashedRefinedType(parent, refinedName, refinedInfo, h)
      if (monitored) recordCaching(h, classOf[PreHashedRefinedType])
      if (h == NotCached) newType
      else {
        val r = findPrevious(h, parent, refinedName, refinedInfo)
        if (r ne null) r else addEntryAfterScan(newType)
      }
    }

    def enterIfNew(rt: RefinedType) = {
      if (monitored) recordCaching(rt)
      if (rt.hash == NotCached) rt
      else {
        val r = findPrevious(rt.hash, rt.parent, rt.refinedName, rt.refinedInfo)
        if (r ne null) r else addEntryAfterScan(rt)
      }
    }
  }
}