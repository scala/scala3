class Foo {
  val self = this
  val x = self.n
  val n = 10       // warn
}