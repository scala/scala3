enum Foo:
  case Bar, Baz

object Foo:
  def render = List(values.tail*).mkString

object Test extends App {
  println(Foo.render)
}
