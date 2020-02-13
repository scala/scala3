import scala.collection.mutable

class Foo {
  private val map: mutable.Map[Int, String] = mutable.Map.empty

  def enter(k: Int, v: String) = map(k) = v
}

class Bar extends Foo {
  override def enter(k: Int, v: String) = ???
  def enterSuper(k: Int, v: String) = super.enter(k, v)

  enter(1, "one")
  enterSuper(1, "one")
  super.enter(2, "two")
}
