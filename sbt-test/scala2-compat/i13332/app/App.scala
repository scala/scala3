import collectionstrawman.*

import scala.deriving.Mirror

def syntheticSumMirror_STATIC = {
  val mIList = summon[Mirror.Of[ListStatic.IList[Int]]]
  type derivedICons = Tuple.Head[mIList.MirroredElemTypes]
  val mICons = summon[Mirror.Of[derivedICons]]
  assert(mICons.fromProduct((1, ListStatic.INil)) == ListStatic.ICons(1, ListStatic.INil))
  assert(mIList.ordinal(ListStatic.ICons(1, ListStatic.INil)) == 0)
  assert(mIList.ordinal(ListStatic.INil) == 1)
}

def syntheticProductMirror_STATIC = {
  val mIPair = summon[Mirror.Of[ListStatic.IPair[Int, String]]]
  assert(mIPair.fromProduct((1, "foo")) == ListStatic.IPair(1, "foo"))
}

def syntheticSingletonMirror_STATIC = {
  val mIUnit = summon[Mirror.Of[ListStatic.IUnit.type]]
  assert(mIUnit.fromProduct(EmptyTuple) eq ListStatic.IUnit)
}

def syntheticSingletonMirror_PATH_DEPENDENT = {
  val m = new ListModule()

  val mIUnit = summon[Mirror.Of[m.IUnit.type]]
  assert(mIUnit.fromProduct(EmptyTuple) eq m.IUnit)
}

@main def Test =
  syntheticSumMirror_STATIC
  syntheticProductMirror_STATIC
  syntheticSingletonMirror_STATIC
  syntheticSingletonMirror_PATH_DEPENDENT
