object Test {
  def foo: Unit = {
    val a: GenericCompanion2[Bar] = null
    val b: GenericCompanion2[Baz] = null
    List(a, b)
  }
}

class GenericCompanion2[+CC[X] <: Foo[X]]

class Foo[A]

class Bar[A] extends Foo[A]
class Baz[A] extends Foo[A]
