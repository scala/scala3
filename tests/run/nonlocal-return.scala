import scala.util.control.NonLocalReturns.*

object Test {
  def has(xs: List[Int], elem: Int) =
    returning {
      for (x <- xs)
        if (x == elem) throwReturn(true)
      false
    }

  trait Animal
  object Dog extends Animal
  object Cat extends Animal

  def animal(arg: Int): Animal = returning {
    if (arg < 0) throwReturn(Dog) 
    Cat
  }

  def main(arg: Array[String]): Unit = {
    assert(has(1 :: 2 :: Nil, 1))
    assert(has(1 :: 2 :: Nil, 2))
    assert(!has(1 :: 2 :: Nil, 3))
    assert(animal(1) == Cat)
    assert(animal(-1) == Dog)
  }
}