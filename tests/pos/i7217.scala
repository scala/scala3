class Foo(p1: String, p2: String):
  def this(p1: String) = this(p1, "blah")

val x = { Foo("blah", "hah") }
val y = { Foo("blah") }
