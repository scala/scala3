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

def toScalaList[T](xs: List[T]) = xs[scala.List[T]]((hd: T) => (tl: scala.List[T]) => hd :: tl)(Nil)

def strictMap[A <: Top, B <: Top](xs: List[A])(strictMapF: (A => B) retains *): List[B] =
  xs[List[B]]((hd: A) => (tl: List[B]) => cons(strictMapF(hd), tl))(nil)

def strictMap2[A <: Top, B <: Top](f: (A => B) retains *): (List[A] => List[B]) retains f.type =
  (xs: List[A]) => xs[List[B]]((hd: A) => (tl: List[B]) => cons(f(hd), tl))(nil)

def pureMap[A <: Top, B <: Top](xs: List[A])(f: A => B): List[B] =
  xs[List[B]]((hd: A) => (tl: List[B]) => cons(f(hd), tl))(nil)

def consForLazyMap[T <: Top](hd: T, tl: List[T] retains *): List[T] retains hd.type retains tl.type =
  [C <: Top] => (op: Op[T, C]) => (s: C) => op(hd)(tl.apply(op)(s))

class Unit
object unit extends Unit

def lazyMap
  [A <: Top, B <: Top]
  (xs: List[(Unit => A) retains A] retains A)
  (f: (A => B) retains *):
  List[(Unit => B) retains A retains B retains f.type] retains A retains B retains f.type =

    xs[List[(Unit => B) retains A retains B retains f.type] retains A retains B retains f.type]
      ((hd: (Unit => A) retains A) =>
        (tl: List[(Unit => B) retains A retains B retains f.type] retains A retains B retains f.type) =>
          consForLazyMap((u: Unit) => f(hd(unit)), tl))(nil)

def force[A](thunk: Unit=>A): A = thunk(unit)
def forceList[A](lazyList: List[Unit=>A]): List[A] = strictMap(lazyList)(force[A])

@main def Test() =
  val list12 = cons(1, cons(2, nil))
  val list23 = strictMap(list12)((_: Int) + 1)
  val list34 = strictMap2((_: Int) + 2)(list12)
  val list45 = pureMap(list12)((_: Int) + 3)
  println(toScalaList(list12))
  println(toScalaList(list23))
  println(toScalaList(list34))
  println(toScalaList(list45))

  val lazylist12: List[Unit=>Int] = cons(unit=>1, cons(unit=>2, nil))
  val lazylist56 = lazyMap[Int, Int](lazylist12)((_: Int) + 4)
  println(toScalaList(forceList(lazylist12)))
  println(toScalaList(forceList(lazylist56)))