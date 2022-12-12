package example
import scala.language.higherKinds

class Anonymous {
  this: Anonymous =>

  def locally[A](x: A): A = x

  def m1[T[_]] = ???
  def m2: Map[_, List[_]] = ???
  locally {
    ??? match { case _: List[_] => }
  }
  locally {
    val x: Int => Int = _ => ???
  }

  trait Foo
  val foo = new Foo {}

  trait Bar:
    def bar: String
  val bar1: Bar = new Bar { def bar: String = ??? }
  val bar2: Bar = new { def bar: String = ??? }
}
