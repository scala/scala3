import scala.collection.mutable

class Foo {
  val map: mutable.Map[Int, String] = mutable.Map.empty

  @scala.annotation.filled
  def enter(k: Int, v: String) = map(k) = v

  def foo(x: Int) = 5 + x
}

class Bar extends Foo {
  enter(1, "one")
  enter(2, "two")

  foo(4)               // error

  val name = "bar"

  foo(4)               // error
}