package listEncoding

type Top = Any retains *
class Cap

type Op[T <: Top, C <: Top] =
  ((v: T) => ((s: C) => C) retains *) retains *

type List[T <: Top] =
  ([C <: Top] => (op: Op[T, C]) => ({op} (s: C) => C))

def nil[T <: Top]: List[T] =
  [C <: Top] => (op: Op[T, C]) => (s: C) => s

def cons[T <: Top](hd: T, tl: List[T]): List[T] =
  [C <: Top] => (op: Op[T, C]) => (s: C) => op(hd)(tl(op)(s))

def foo(c: {*} Cap) =
  def f(x: String retains c.type, y: String retains c.type) =
    cons(x, cons(y, nil))
  def g(x: String retains c.type, y: Any) =
    cons[{c} Any](x, cons[Any](y, nil))  // TODO: drop type arguments
  def h(x: String, y: Any retains c.type) =
    cons(x, cons(y, nil))
