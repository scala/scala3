package collections

object collections {
  type AnyConstr[X] = Any
}

import collections.*

trait Iterable[+A] extends IterableOps[A, Iterable, Iterable[A]]

trait IterableOps[+A, +CC[_], +C]

trait Map[K, +V] extends MapOps[K, V, Map, Map[K, V]]

trait MapOps[K, +V, +CC[_, _] <: IterableOps[_, AnyConstr, _], +C]
  extends IterableOps[(K, V), Iterable, C] {
  def view: MapView[K, V] = ???
}

trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]]

trait MapView[K, +V]
  extends MapOps[K, V, ({ type l[X, Y] = View[(X, Y)] })#l, View[(K, V)]]
  with View[(K, V)]

class MapDecorator[C, M <: HasMapOps[C]]

trait HasMapOps[C] {

  /** The type of keys */
  type K

  /** The type of values */
  type V

  type A = (K, V)
}

object decorators {
  def MapDecorator[C](coll: C)(implicit map: HasMapOps[C]): MapDecorator[C, map.type] = ???

  implicit def mapHasMapOps[CC[X, +Y] <: MapOps[X, Y, ({ type l[X, +Y] = IterableOps[_, AnyConstr, _] })#l, _], K0, V0]: HasMapOps[CC[K0, V0]] { type K = K0; type V = V0 } = ???
}