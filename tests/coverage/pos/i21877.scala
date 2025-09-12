

class Schema[T]
object Schema {
  inline def derived[T]: Schema[T] = new Schema[T]
}

case class Bar(x: Int)

object Foo {
  def foo(x: Int): String = {
    val bar = Bar(x)
    if (x == 5) "5" else "idk" + bar.toString
  }

  implicit val schema: Schema[Bar] = Schema.derived
}

@main def test() =
  Foo.foo(4)
  Foo.foo(5)
