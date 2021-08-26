import scala.deriving.Mirror
import scala.CanEqual.derived

class Outer {

  sealed trait Item
  case class Fruit(seed: Seed) extends Item
  case class Seed() extends Item

  final lazy val o = new Outer() // infinite init

  def hello: Unit = {

    val mFruit = summon[Mirror.Of[o.Item & o.Fruit]]
    type derivedSeed = Tuple.Head[mFruit.MirroredElemTypes]
    val mSeed = summon[Mirror.Of[derivedSeed]]

    assert(mFruit.fromProduct(Tuple(o.Seed())) == o.Fruit(o.Seed()))
    assert(mSeed.fromProduct(EmptyTuple) == o.Seed()) // careful to ensure that correct outer is captured
  }

}

@main def Test = {
  val o = new Outer()
  o.hello
}
