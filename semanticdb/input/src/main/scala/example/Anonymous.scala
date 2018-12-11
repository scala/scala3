package example
import scala.language.higherKinds

class Anonymous {
  this: Anonymous =>

  def m1[T[_], B] = ???
  def m2: Map[_, List[_]] = ???
  locally {
    ??? match { case _: List[_] => }
  }
  locally {
    val x: Int => Int = _ => ???
  }

  trait Foo
  var x = new Foo {}
}
