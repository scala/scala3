import scala.deriving.Mirror

trait Mixin
object Mixin

trait Parent
object Parent

sealed trait Fruit extends Parent
object Fruit {
  case object Apple extends Fruit
  case object Orange extends Fruit
}

@main def Test = {
  val mFruit = summon[Mirror.SumOf[Fruit & Parent]]
  assert(mFruit.ordinal(Fruit.Apple) == 0)
  assert(mFruit.ordinal(Fruit.Orange) == 1)
}
