//> using options -Yexplicit-nulls -Wsafe-init

import scala.annotation.retains
import scala.collection.{AbstractSet, IterableFactory, IterableOps, Set, View}
import scala.language.experimental.captureChecking

transparent trait MiniMapOps[K, +V, +C]
extends IterableOps[(K, V), Iterable, C]
  with PartialFunction[K, V]:
  self: MiniMapOps[K, V, C]^ =>

  override def view: View[(K, V)]^{this} = View.from(this)

  def iterator: Iterator[(K, V)] = Iterator.empty
  def iterableFactory: IterableFactory[Iterable] = Iterable
  def get(key: K): Option[V]

  def apply(key: K): V = get(key).get

  protected trait GenKeySet @retains[MiniMapOps.this.type]():
    this: Set[K] =>
    def contains(key: K): Boolean =
      scala.caps.unsafe.unsafeDiscardUses(MiniMapOps.this).contains(key)

  def keySet: Set[K] = new AbstractSet[K] with GenKeySet:
    def iterator: Iterator[K] = Iterator.empty
    def diff(that: Set[K]): Set[K] = this

  def contains(key: K): Boolean =
    scala.caps.unsafe.unsafeDiscardUses(this.get(key)).isDefined

  def isDefinedAt(key: K): Boolean = contains(key)
