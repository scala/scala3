import scala.deriving.Mirror

class Outer { outer =>

  object Inner {
    sealed trait Item // both children and parent share a common sub-prefix
  }

  object Wrapper {
    case object A extends outer.Inner.Item
    case object B extends outer.Inner.Item
  }

}

@main def Test: Unit = {

  val o = new Outer()

  val mItem = summon[Mirror.Of[o.Inner.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(o.Wrapper.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == o.Wrapper.A)

}
