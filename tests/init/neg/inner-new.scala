class Foo {
  class Inner {
    def f() = count + 1
  }

  val a = new Inner   // ok
  println(a)          // error

  var count = 0
  println(a)          // ok
}