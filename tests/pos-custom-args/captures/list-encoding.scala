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

def toScalaList[T](xs: List[T]) = xs[scala.List[T]](hd => (tl: scala.List[T]) => hd :: tl)(Nil)

def strictMap[A <: Top, B <: Top](xs: List[A])(f: (A => B) retains *): List[B] =
  xs[List[B]](hd => (tl: List[B]) => cons(f(hd), tl))(nil)

def strictMap2[A <: Top, B <: Top](f: (A => B) retains *): (List[A] => List[B]) retains f.type =
  (xs: List[A]) => xs[List[B]](hd => (tl: List[B]) => cons(f(hd), tl))(nil)

def pureMap[A <: Top, B <: Top](xs: List[A])(f: A => B): List[B] =
  xs[List[B]](hd => (tl: List[B]) => cons(f(hd), tl))(nil)

def consForLazyMap[T <: Top](hd: T, tl: List[T] retains *): List[T] =
  [C <: Top] => (op: Op[T, C]) => (s: C) => op(hd)(tl(op)(s))

def lazyMap
  [A <: Top, B <: Top]
  (xs: List[(Unit => A) retains A] retains A)
  (f: (A => B) retains *):
  List[(Unit => B) retains A retains B retains f.type] retains A retains B retains f.type =

    xs[List[(Unit => B) retains A retains B retains f.type] retains A retains B retains f.type]
      (hd =>
        (tl: List[(Unit => B) retains A retains B retains f.type] retains A retains B retains f.type) =>
          consForLazyMap((u: Unit) => f(hd(())), tl))(nil)

@main def Test() =
  val l = cons(1, cons(2, nil))
  val l2 = strictMap(l)((_: Int) + 1)
  val l3 = strictMap2((_: Int) + 2)(l)
  val l4 = pureMap(l)((_: Int) + 3)
  println(toScalaList(l))
  println(toScalaList(l2))
  println(toScalaList(l3))