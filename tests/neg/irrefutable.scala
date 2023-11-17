// This tests that A.f1 is recognized as an irrefutable pattern and A.f2_nocase is not, and therefore A.f2 solves this
// by adding a case to the pattern, which results in withFilter being inserted.
// see also: tests/run/irrefutable.scala for an example that exercises the insertion of withFilter.

class Lst[+T](val id: String, val underlying: List[T]) {
  def map[U](f: T => U): Lst[U] = new Lst(id, underlying.map(f))

  // hide the withFilter so that there is a compile error
  // def withFilter(f: T => Boolean): Lst.WithFilter[T] = new Lst.WithFilter(this, f)
}

// object Lst:
//   class WithFilter[+T](lst: Lst[T], filter: T => Boolean):
//     def forwardingFilter[T1](filter: T1 => Boolean): T1 => Boolean = t =>
//       println(s"filtering $t in ${lst.id}")
//       filter(t)

//     def map[U](f: T => U): Lst[U] = Lst(lst.id, lst.underlying.withFilter(forwardingFilter(filter)).map(f))

case class Foo[T](x: T)

object A {
  def f1(xs: Lst[Foo[Int]]): Lst[Int] = {
    for (Foo(x: Int) <- xs) yield x
  }
  def f2(xs: Lst[Foo[Any]]): Lst[Int] = {
    for (case Foo(x: Int) <- xs) yield x // error
  }
  def f2_nocase(xs: Lst[Foo[Any]]): Lst[Int] = {
    for (Foo(x: Int) <- xs) yield x // error
  }
}

@main def Test =
  val xs = new Lst("xs", List(Foo(1), Foo(2), Foo(3)))
  println("=== mapping xs with A.f1 ===")
  val xs1 = A.f1(xs)
  assert(xs1.underlying == List(1, 2, 3))
  val ys = new Lst("ys", List(Foo(1: Any), Foo(2: Any), Foo(3: Any)))
  println("=== mapping ys with A.f2 ===")
  val ys1 = A.f2(ys)
  assert(ys1.underlying == List(1, 2, 3))
