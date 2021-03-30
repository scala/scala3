// Test that array contents are non-nullable.
class Foo {
  val x: Array[String] = Array("hello")
  val s: String = x(0)

  def test = {
    // accept any array of string
    def f(xs: Array[_ >: String <: String | Null] | Null): Unit = ???

    val a1: Array[String] = ???
    val a2: Array[String] | Null = ???
    val a3: Array[String | Null] = ???
    val a4: Array[String | Null] | Null = ???

    f(null)
    f(Array())
    f(Array(null))
    f(Array(""))
    f(Array("", null))
    f(a1)
    f(a2)
    f(a3)
    f(a4)
  }
}
