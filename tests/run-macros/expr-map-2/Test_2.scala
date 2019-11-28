object Test {

  def main(args: Array[String]): Unit = {
    println(rewrite(new Foo(2)))
    println(rewrite(new Foo(2).x))

    rewrite {
      val foo = new Foo(2)
      println(foo.x)
    }

  }
}
