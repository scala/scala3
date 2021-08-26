import scala.deriving.Mirror

class Outer {

  enum Item {
    case A, B
  }

}

@main def Test: Unit = {

  val o = new Outer()

  val mItem = summon[Mirror.Of[o.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(o.Item.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == o.Item.A)

}
