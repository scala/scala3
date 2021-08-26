import scala.deriving.Mirror

class Outer { self =>

  sealed trait Item
  object Item {
    sealed trait Fruit extends Item
    object Fruit {
      case object Apple extends Item with Fruit
      case object Orange extends Item with Fruit
    }
  }

  lazy val o = new Outer() // infinite init

  def hello: Unit = {
    val mFruit = summon[Mirror.Of[o.Item & o.Item.Fruit]]
    type derivedApple = Tuple.Head[mFruit.MirroredElemTypes]
    val mApple = summon[Mirror.Of[derivedApple]]

    assert(mFruit.ordinal(o.Item.Fruit.Apple) == 0)
    assert(mApple.fromProduct(EmptyTuple) == o.Item.Fruit.Apple)
  }

}

@main def Test = {
  val o = new Outer()
  o.hello
}
