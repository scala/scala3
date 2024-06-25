import scala.collection.mutable

abstract class Foo {
  private val map: mutable.Map[Int, String] = mutable.Map.empty

  def enter(k: Int, v: String) = map(k) = v

  def name: String
  def foo(x: Int) = 5 + name.size
}

class Bar extends Foo {
  enter(1, "one")
  enter(2, "two")

  foo(4)

  val name = "bar"  // warn

  foo(4)
}