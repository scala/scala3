trait VAL

type Foo[T, M] = M match { case VAL => T }

class Cand[A]
object Cand:
  given inst[X, Y <: Foo[X, VAL]]: Cand[Y] = new Cand[Y]
