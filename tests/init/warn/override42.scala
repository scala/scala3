class Foo {
  val name = "child"

  println(show)

  def show = println(name)
}


class Bar {
  println(show)

  def show = println(name)

  val name = "child"    // warn
}
