object Test {
  def main(args: Array[String]): Unit = {
    val jl = classOf[Foo].getMethod("jl", classOf[Baz[_]])
    jl.getGenericParameterTypes // works fine

    val l = classOf[Foo].getMethod("l", classOf[Baz[_]])
    l.getGenericParameterTypes // works fine
  }
}

class Baz[T]

class Foo {
  def l(b: Baz[Long]): Unit = { }
  def jl(b: Baz[java.lang.Long]): Unit = { }
}
