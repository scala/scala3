//> using options -experimental

import scala.collection.BuildFrom

def diffs(ns: IArray[Long]): IArray[Long] =
  ns.lazyZip(ns.tail).map((a, b) => b - a)

def diffsB(ns: IArray[Long])(bf: BuildFrom[IArray[Long], Long, IArray[Long]]): IArray[Long] =
  ns.lazyZip(ns.tail).map((a, b) => b - a)(using bf)

@main def Test: Unit =
  val explicit: BuildFrom[IArray[Long], Long, IArray[Long]] =
    scala.collection.BuildFrom.buildFromIArray[Long]

  val contextual: BuildFrom[IArray[Long], Long, IArray[Long]] =
    summon[BuildFrom[IArray[Long], Long, IArray[Long]]]

  val contextualWide: BuildFrom[IArray[Any], Long, IArray[Long]] =
    summon[BuildFrom[IArray[Any], Long, IArray[Long]]]

  assert(explicit.getClass == contextual.getClass) // check default is same implementation.
  assert(explicit.getClass == contextualWide.getClass) // check default is same implementation.

  assert(diffs(IArray(1L, 3L, 6L, 10L)).sameElements(IArray(2L, 3L, 4L)))
  assert(diffsB(IArray(1L, 3L, 6L, 10L))(contextual).sameElements(IArray(2L, 3L, 4L)))
  assert(diffsB(IArray(1L, 3L, 6L, 10L))(contextualWide).sameElements(IArray(2L, 3L, 4L)))
