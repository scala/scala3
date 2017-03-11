import java.util
object Test {
  def main(args: Array[String]): Unit = {
    println(new Bar[Int].foo(Array(1, 2, 3), 5).mkString)
  }
}

class Bar[E] extends Foo[E] {
  def foo[T](a: Array[T], e: T): Array[T] = {
    a(0) = e
    a
  }
}
