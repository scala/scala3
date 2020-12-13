package dotty.tools.dotc
package core

import Types._, Contexts._, util.Stats._, Hashable._, Names._
import config.Config
import Decorators._
import util.{HashSet, Stats}

class Uniques extends HashSet[Type](Config.initialUniquesCapacity):
  override def hash(x: Type): Int = x.hash
  override def isEqual(x: Type, y: Type) = x.eql(y)

/** Defines operation `unique` for hash-consing types.
 *  Also defines specialized hash sets for hash consing uniques of a specific type.
 *  All sets offer a `enterIfNew` method which checks whether a type
 *  with the given parts exists already and creates a new one if not.
 */
object Uniques:

  private inline def recordCaching(tp: Type): Unit = recordCaching(tp.hash, tp.getClass)
  private inline def recordCaching(h: Int, clazz: Class[?]): Unit =
    if monitored then
      if h == NotCached then
        record("uncached-types")
        record(s"uncached: $clazz")
      else
        record("cached-types")
        record(s"cached: $clazz")

  def unique[T <: Type](tp: T)(using Context): T =
    recordCaching(tp)
    if tp.hash == NotCached then tp
    else ctx.uniques.put(tp).asInstanceOf[T]

  final class NamedTypeUniques extends HashSet[NamedType](Config.initialUniquesCapacity * 4) with Hashable:
    override def hash(x: NamedType): Int = x.hash

    def enterIfNew(prefix: Type, designator: Designator, isTerm: Boolean)(using Context): NamedType =
      val h = doHash(null, designator, prefix)
      if monitored then recordCaching(h, classOf[NamedType])
      def newType =
        if (isTerm) new CachedTermRef(prefix, designator, h)
        else new CachedTypeRef(prefix, designator, h)
      if h == NotCached then newType
      else
        Stats.record(statsItem("put"))
        var idx = index(h)
        var e = entryAt(idx)
        while e != null do
          if (e.prefix eq prefix) && (e.designator eq designator) && (e.isTerm == isTerm) then return e
          idx = nextIndex(idx)
          e = entryAt(idx)
        addEntryAt(idx, newType)
  end NamedTypeUniques

  final class AppliedUniques extends HashSet[AppliedType](Config.initialUniquesCapacity * 2) with Hashable:
    override def hash(x: AppliedType): Int = x.hash

    def enterIfNew(tycon: Type, args: List[Type]): AppliedType =
      val h = doHash(null, tycon, args)
      def newType = new CachedAppliedType(tycon, args, h)
      if monitored then recordCaching(h, classOf[CachedAppliedType])
      if h == NotCached then newType
      else
        Stats.record(statsItem("put"))
        var idx = index(h)
        var e = entryAt(idx)
        while e != null do
          if (e.tycon eq tycon) && e.args.eqElements(args) then return e
          idx = nextIndex(idx)
          e = entryAt(idx)
        addEntryAt(idx, newType)
  end AppliedUniques
end Uniques
