import java.util.function.Function

object Test {
  def foo[V](v: V): Int = 1
  def foo[U](fn: Function[Int, U]): Int = 2

  def foo2(a: Int => Int): Int = 1
  def foo2(a: PartialFunction[Int, Int]): Int = 2

  def main(args: Array[String]): Unit = {
    assert(foo((x: Int) => x) == 2)
    val jf: Function[Int, Int] = x => x
    assert(foo(jf) == 2)

    assert(foo2(x => x) == 1)
    val f: Int => Int = x => x
    assert(foo2(f) == 1)
    assert(foo2({ case x if x % 2 == 0 => x }) == 2)
    val pf: PartialFunction[Int, Int] = { case x if x % 2 == 0 => x }
    assert(foo2(pf) == 2)
  }
}
