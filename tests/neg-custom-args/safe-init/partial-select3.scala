class Foo {
  val name = "child"

  println(show)

  def show = println(name)
}


class Bar {
  println(show)        // error

  def show = println(name)   // error

  val name = "child"
}
