import scala.deriving.Mirror

class Outer { self =>

  object Inner {
    sealed trait Item // both children and parent share a common sub-prefix
  }

  object Wrapper {
    case object A extends self.Inner.Item
    case object B extends self.Inner.Item
  }

  val mItem = summon[Mirror.Of[self.Inner.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(self.Wrapper.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == self.Wrapper.A)

}

@main def Test = {
  new Outer()
}
