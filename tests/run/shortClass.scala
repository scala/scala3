import scala.reflect.internal.util._

package bippity {
  trait DingDongBippy

  package bop {
    class Foo {
      class Bar
      object Bar
    }
  }
}

object Test {
  import bippity._
  import bop._

  def printSanitized(x: String) = println(x.filterNot(_.isDigit))

  def main(args: Array[String]): Unit = {
    val f = new Foo
    val instances = List(f, new f.Bar, f.Bar, new Foo with DingDongBippy, new f.Bar with DingDongBippy)
    instances map (_.getClass.getName) foreach printSanitized
    instances map shortClassOfInstance foreach printSanitized
  }
}
