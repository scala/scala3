class Foo {
  val f: Int => Foo = (x: Int) => if x > 0 then f(x) else this
  f(10).n

  val n = 10   // warn
}