final class Foo {
  def fun: Int => Int = n => n + x.size
  fun(5)

  val x = "hello"   // warn
}