
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    printTypes {
      val x = 1
      val y: x.type = x
      def f1[T](): T = ???
      def f2[T >: Int <: Int](): T = ???
      class Foo { type X }
      val foo = new Foo { type X = String }
    }
  }
}

trait Foo
trait Bar
class Baz extends Foo with Bar
