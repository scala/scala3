import scala.deriving.Mirror

class Outer {

  sealed trait Item // children have identical prefix to parent
  case object A extends Item
  case object B extends Item

}

@main def Test: Unit = {

  val o = new Outer()

  val mItem = summon[Mirror.Of[o.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(o.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == o.A)

}
