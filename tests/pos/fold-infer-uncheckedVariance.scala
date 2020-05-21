import scala.annotation.unchecked.uncheckedVariance

class Bar

class Foo[-T >: Null] {
  def tpe: T @uncheckedVariance = ???
}

object Test {
  def foo(xs: List[Int], x: Foo[Bar]): Unit = {
    val r = xs.foldLeft(x) { (acc, x) =>
      // B >: Foo[Bar] <: Foo[T]
      // T >: Null <: Bar
      val m = acc.tpe
      acc
    }
    val s: Foo[Bar] = r
  }
}
