trait Foo[CP <: NonEmptyTuple]:
  type EndNode = Tuple.Last[CP]

def f(end: Foo[?]): end.EndNode =
  ???

trait Bar[CP <: NonEmptyTuple] extends Foo[CP]:
  val v: EndNode = f(this)
