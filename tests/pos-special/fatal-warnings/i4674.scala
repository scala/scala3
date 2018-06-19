class Foo {
  def foo: Unit = {
    val x: String = "something"
    x.foreach {
      case 's' => println("s")
      case c: Char => println(c) // no warn for `: Char` allways succeeding
    }
  }
}
