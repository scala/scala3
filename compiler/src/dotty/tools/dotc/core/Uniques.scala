package dotty.tools.dotc
package core

import Types._, Contexts._, util.Stats._, Hashable._, Names._
import config.Config
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

  final class NamedTypeUniques extends HashSet[NamedType](Config.initialUniquesCapacity) with Hashable {
    override def hash(x: NamedType): Int = x.hash

    private def findPrevious(h: Int, prefix: Type, name: Name): NamedType = {
      var e = findEntryByHash(h)
      while (e != null) {
        if ((e.prefix eq prefix) && (e.name eq name)) return e
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

  final class TypeAliasUniques extends HashSet[TypeAlias](Config.initialUniquesCapacity) with Hashable {
    override def hash(x: TypeAlias): Int = x.hash

    private def findPrevious(h: Int, alias: Type, variance: Int): TypeAlias = {
      var e = findEntryByHash(h)
      while (e != null) {
        if ((e.alias eq alias) && (e.variance == variance)) return e
        e = nextEntryByHash(h)
      }
      e
    }

    def enterIfNew(alias: Type, variance: Int): TypeAlias = {
      val h = doHash(variance, alias)
      if (monitored) recordCaching(h, classOf[TypeAlias])
      def newAlias = new CachedTypeAlias(alias, variance, h)
      if (h == NotCached) newAlias
      else {
        val r = findPrevious(h, alias, variance)
        if (r ne null) r
        else addEntryAfterScan(newAlias)
      }
    }
  }


  final class AppliedUniques extends HashSet[AppliedType](Config.initialUniquesCapacity) with Hashable {
    override def hash(x: AppliedType): Int = x.hash

    private def findPrevious(h: Int, tycon: Type, args: List[Type]): AppliedType = {
      var e = findEntryByHash(h)
      while (e != null) {
        def sameArgs(args1: List[Type], args2: List[Type]): Boolean = {
          val empty1 = args1.isEmpty
          val empty2 = args2.isEmpty
          if (empty1) empty2
          else (!empty2 && (args1.head eq args2.head) && sameArgs(args1.tail, args2.tail))
        }
        if ((e.tycon eq tycon) && sameArgs(e.args, args)) return e
        e = nextEntryByHash(h)
      }
      e
    }

    def enterIfNew(tycon: Type, args: List[Type]): AppliedType = {
      val h = doHash(tycon, args)
      def newType = new CachedAppliedType(tycon, args, h)
      if (monitored) recordCaching(h, classOf[CachedAppliedType])
      if (h == NotCached) newType
      else {
        val r = findPrevious(h, tycon, args)
        if (r ne null) r else addEntryAfterScan(newType)
      }
    }
  }

  final class RefinedUniques extends HashSet[RefinedType](Config.initialUniquesCapacity) with Hashable {
    override val hashSeed = classOf[CachedRefinedType].hashCode // some types start life as CachedRefinedTypes, need to have same hash seed
    override def hash(x: RefinedType): Int = x.hash

    private def findPrevious(h: Int, parent: Type, refinedName: Name, refinedInfo: Type): RefinedType = {
      var e = findEntryByHash(h)
      while (e != null) {
        if ((e.parent eq parent) && (e.refinedName eq refinedName) && (e.refinedInfo eq refinedInfo))
          return e
        e = nextEntryByHash(h)
      }
      e
    }

    def enterIfNew(parent: Type, refinedName: Name, refinedInfo: Type): RefinedType = {
      val h = doHash(refinedName, refinedInfo, parent)
      def newType = new CachedRefinedType(parent, refinedName, refinedInfo, h)
      if (monitored) recordCaching(h, classOf[CachedRefinedType])
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
