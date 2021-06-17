type Top = Any retains *
class Cap extends Retains[*]

type Op[T <: Top, C <: Top] =
  ((v: T) => ((s: C) => C) retains *) retains *

type List[T <: Top] =
  ([C <: Top] => (op: Op[T, C]) => ((s: C) => C) retains op.type) retains T

def nil[T <: Top]: List[T] =
  [C <: Top] => (op: Op[T, C]) => (s: C) => s

def cons[T <: Top](hd: T, tl: List[T]): List[T] =
  [C <: Top] => (op: Op[T, C]) => (s: C) => op(hd)(tl(op)(s))

def foo(c: Cap) =
  def f(x: String retains c.type, y: String retains c.type) =
    cons(x, cons(y, nil))
  def g(x: String retains c.type, y: Any) =
    cons(x, cons(y, nil))
  def h(x: String, y: Any retains c.type) =
    cons(x, cons(y, nil))

def toScalaList[T](l: List[T]) = l[scala.List[T]](hd => (tl: scala.List[T]) => hd :: tl)(Nil)

def map[A, B](l: List[A])(f: (A => B) retains *): List[B] =
  l[List[B]](hd => (tl: List[B]) => cons(f(hd), tl))(nil)

def map2[A, B](f: (A => B) retains *): (List[A] => List[B]) retains f.type =
  (l: List[A]) => l[List[B]](hd => (tl: List[B]) => cons(f(hd), tl))(nil)

@main def Test() =
  val l = cons(1, cons(2, nil))
  val l2 = map(l)((_: Int) + 1)
  println(toScalaList(l))
  println(toScalaList(l2))