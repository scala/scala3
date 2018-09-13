trait Foo {
  def apply[~>[_,_]](x: Int ~> Int): Int ~> Int
}

object Foo {
  inline def foo: Foo = new Foo {
    def apply[~>[_,_]](x: Int ~> Int): Int ~> Int = x
  }

  def main(args: Array[String]): Unit = {
    val x = foo((x: Int) => x)
  }
}
