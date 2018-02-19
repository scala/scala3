import java.util.Comparator

trait Trait1[T] { def foo(arg: Comparator[T]): String }

trait Trait2[T] extends Trait1[T] { def foo(arg: Comparator[String]): Int = 0 }

class Class1 extends Trait2[String] { } // error: not a legal implementation of `foo'

object Test {
  def main(args: Array[String]): Unit = {
    val c = new Class1
    c.foo(Ordering[String])
    val t: Trait1[String] = c
    val x: String = t.foo(Ordering[String])
  }
}
