package example

// https://github.com/scala/scala3/issues/26521
object Foo {
  def build(): Int = {
    final case class Local(a: Int, b: Int)
    val x = Local(a = 1, b = 2)
    x.a + x.b
  }
}
