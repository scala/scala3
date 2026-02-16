package listEncoding
import annotation.retains

class Cap

type Op[T, C] =
  T => C => C


class STR // used to be pure class String but then the errors don't show up.

type List[T] =
  [C] -> (op: Op[T, C]) -> (s: C) ->{op} C

def nil[T]: List[T] =
  [C] => (op: Op[T, C]) => (s: C) => s

def cons[T](hd: T, tl: List[T]): List[T] =
  [C] => (op: Op[T, C]) => (s: C) => op(hd)(tl(op)(s))

def foo(c: Cap^) =
  def f(x: STR @retains[c.type], y: STR @retains[c.type]) =
    cons(x, cons(y, nil))  // error, should this work?
  def f_explicit(x: STR @retains[c.type], y: STR @retains[c.type])
    : [C] => (op: STR^{x, y} => C => C) -> (s: C) ->{op} C =
    cons(x, cons(y, nil))  // error, should this work?
  def g(x: STR @retains[c.type], y: Any) =
    cons(x, cons(y, nil))
  def h(x: STR, y: Any @retains[c.type]) =
    cons(x, cons(y, nil))
