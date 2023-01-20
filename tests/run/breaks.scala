import scala.util.boundary, boundary.break
import collection.mutable.ListBuffer

object Test {
  def has(xs: List[Int], elem: Int) =
    boundary:
      for x <- xs do
        if x == elem then break(true)
      false

  def takeUntil(xs: List[Int], elem: Int) =
    boundary:
      var buf = new ListBuffer[Int]
      for x <- xs yield
        if x == elem then break(buf.toList)
        buf += x
        x

  trait Animal
  object Dog extends Animal
  object Cat extends Animal

  def animal(arg: Int): Animal =
    boundary:
      if arg < 0 then break(Dog)
      Cat

  def main(arg: Array[String]): Unit = {
    assert(has(1 :: 2 :: Nil, 1))
    assert(has(1 :: 2 :: Nil, 2))
    assert(!has(1 :: 2 :: Nil, 3))
    assert(animal(1) == Cat)
    assert(animal(-1) == Dog)

    assert(has(List(1, 2, 3), 2))
    assert(takeUntil(List(1, 2, 3), 3) == List(1, 2))
  }
}