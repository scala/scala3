import scala.collection.mutable

class Foo {
  private val map: mutable.Map[Int, String] = mutable.Map.empty

  def enter(k: Int, v: String) = map(k) = v
}

class Bar extends Foo {
  enter(1, "one")
  enter(2, "two")
}

class Bar2 extends Bar {
  val mymap: mutable.Map[Int, String] = mutable.Map.empty  // warn

  override def enter(k: Int, v: String) = {
    mymap(k) = v
  }
}
