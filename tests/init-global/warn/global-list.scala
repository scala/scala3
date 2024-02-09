case class Foo(name: String)

object O: // warn
  val a = Foo("Apple")
  val b = Foo("Banana")
  val c = Foo("Cherry")

object Foo:
  val all: List[Foo] = List(O.a, O.b, O.c) // warn // warn // warn