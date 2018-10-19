
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    printOwners {
      def foo = {
        def bar = 1
        val bar2 = 2
        bar
      }
      val foo2 = {
        def baz = 3
        val baz2 = 4
        baz
      }
      class A {
        type B = Int
        def b = 5
        val b2 = 6
      }
    }
  }
}
