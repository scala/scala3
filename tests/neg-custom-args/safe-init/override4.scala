import scala.annotation.partial
import scala.annotation.filled
import scala.collection.mutable

class Foo {
  val map: mutable.Map[Int, String] = mutable.Map.empty

  @filled
  def enter(k: Int, v: String) = map(k) = v
}

class Bar extends Foo {
  enter(1, "one")
  enter(2, "two")
}

class Bar2 extends Bar {
  val mymap: mutable.Map[Int, String] = mutable.Map.empty

  override def enter(k: Int, v: String) = {   // error
    mymap(k) = v                   // error
  }
}

class Foo1 {
  val map: mutable.Map[Int, String] = mutable.Map.empty

  @partial
  def enter(k: Int, v: String) = map(k) = v // error // error
}


abstract class Foo2 {
  def map: mutable.Map[Int, String]

  @partial
  def enter(k: Int, v: String) = map(k) = v  // error // error
}