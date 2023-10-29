type MT[T, M] = M match { case VAL => T }

class Foo[A]
object Foo:
  given inst[X, Y <: MT[X, VAL]]: Foo[Y] = new Foo[Y]

trait VAL
