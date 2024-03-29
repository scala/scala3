class Foo {
  case class Inner(x: Int) {
    def f() = count + x
  }

  val a = Inner(5)    // ok
  println(a)          // warn

  var count = 0
  println(a)          // ok
}