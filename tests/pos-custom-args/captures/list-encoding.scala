package listEncoding

class Cap

type Op[T, C] =
  {*} (v: T) => {*} (s: C) => C

type List[T] =
  [C] => (op: Op[T, C]) => {op} (s: C) => C

def nil[T]: List[T] =
  [C] => (op: Op[T, C]) => (s: C) => s

def cons[T](hd: T, tl: List[T]): List[T] =
  [C] => (op: Op[T, C]) => (s: C) => op(hd)(tl(op)(s))

def foo(c: {*} Cap) =
  def f(x: String @retains(c), y: String @retains(c)) =
    cons(x, cons(y, nil))
  def g(x: String @retains(c), y: Any) =
    cons(x, cons(y, nil))
  def h(x: String, y: Any @retains(c)) =
    cons(x, cons(y, nil))
