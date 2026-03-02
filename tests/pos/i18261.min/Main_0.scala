type Id[T] = Any match { case Any => T }

class Foo[A]
object Foo:
  given inst: [X, Y <: Id[X]] => Foo[Y] = new Foo[Y]
