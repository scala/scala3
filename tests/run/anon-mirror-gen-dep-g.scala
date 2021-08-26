import scala.deriving.Mirror

class Outer {

  sealed trait Item
  case object A extends Item
  case object B extends Item

  lazy val o = new Outer() // infinite init

  def hello: Unit = {

    val mItem = summon[Mirror.Of[o.Item]]
    type derivedA = Tuple.Head[mItem.MirroredElemTypes]
    val mA = summon[Mirror.Of[derivedA]]

    assert(mItem.ordinal(o.A) == 0)
    assert(mA.fromProduct(EmptyTuple) == o.A)

  }

}

@main def Test = {
  val o = new Outer()
  o.hello
}
