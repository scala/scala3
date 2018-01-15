class Foo {
  def foo: Unit = {
    val expr ='{
      val a = 3
      println("foo")
      2 + a
    }
  }
}
