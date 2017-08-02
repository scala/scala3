object Foo extends (Int => Int) {
  inline def apply(x: Int): Int = impl(x)
  def impl(x: Int): Int = x + 1
}

object Test {

  def test(foo: Foo.type): Int = foo(41)

  def test2(f: Int => Int): Int = f(41)

  def main(args: Array[String]): Unit = {
    assert(test(Foo) == 42)
    assert(test2(Foo) == 42)
  }

}
