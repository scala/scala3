final class Foo(val value: Int)

object Foo {
  def unapplySeq(foo: Foo): (Int, Seq[Int]) = (foo.value, Nil)
}

object Test {
  def main(args: Array[String]): Unit = {
    (new Foo(3)) match {
      case Foo(x, _*) =>
        assert(x == 3)
    }
  }
}
