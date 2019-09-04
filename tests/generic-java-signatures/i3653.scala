class Foo {
  def bar[T] =
    (a0: T, a1: T, a2: T, a3: T, a4: T, a5: T, a6: T, a7: T, a8: T, a9: T,
     b0: T, b1: T, b2: T, b3: T, b4: T, b5: T, b6: T, b7: T, b8: T, b9: T,
     c0: T, c1: T, c2: T, c3: T, c4: T, c5: T, c6: T, c7: T, c8: T, c9: T) => 0

  // #6946
  def baz = (x: given String => Unit) => x given ""
}

object Test {
  def main(args: Array[String]): Unit = {
    val meth1 = classOf[Foo].getDeclaredMethod("bar")
    println(meth1.toGenericString)
    val meth2 = classOf[Foo].getDeclaredMethod("baz")
    println(meth2.toGenericString)
  }
}
