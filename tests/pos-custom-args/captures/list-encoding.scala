class Cap
type Top = Any @retains(*)

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

def toScalaList[T](xs: List[T]) = xs[scala.List[T]]((hd: T) => (tl: scala.List[T]) => hd :: tl)(Nil)

def strictMap[A <: Top, B <: Top](xs: List[A])(f: {*} A => B): List[B] =
  xs[List[B]]((hd: A) => (tl: List[B]) => cons(f(hd), tl))(nil)

def strictMap2[A <: Top, B <: Top](f: {*} A => B): {f} List[A] => List[B] =
  (xs: List[A]) => xs[List[B]]((hd: A) => (tl: List[B]) => cons(f(hd), tl))(nil)

def pureMap[A <: Top, B <: Top](xs: List[A])(f: A => B): List[B] =
  xs[List[B]]((hd: A) => (tl: List[B]) => cons(f(hd), tl))(nil)

class Unit
object unit extends Unit

def lazyMap
  [A <: Top, B <: Top]
  (xs: List[Unit => A])
  (f: {*} A => B):
  List[{f} Unit => B] =

    xs[List[{f} Unit => B]]
      ((hd: Unit => A) =>
        (tl: List[{f} Unit => B]) =>
          cons((u: Unit) => f(hd(unit)), tl))(nil)

def lazyPureMap
  [A <: Top, B <: Top]
  (xs: List[Unit => A])
  (f: A => B):
  List[Unit => B] =

    xs[List[Unit => B]]
      ((hd: Unit => A) =>
        (tl: List[Unit => B]) =>
          cons((u: Unit) => f(hd(unit)), tl))(nil)

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
  // Uncommenting the next line fails pickling (???)
  // val lazylist56 = lazyMap[Int, Int](lazylist12)((_: Int) + 4)
  val lazylist67 = lazyPureMap[Int, Int](lazylist12)((_: Int) + 5)
  println(toScalaList(forceList(lazylist12)))
  // println(toScalaList(forceList(lazylist56)))
  println(toScalaList(forceList(lazylist67)))
