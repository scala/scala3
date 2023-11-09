class Foo[+T[_]]:
  def combine[T1[x] >: T[x]](x: T1[Int]): Foo[T1] = new Foo
object Foo:
  def empty: Foo[Nothing] = new Foo

object X:
  def test(xs: List[List[Int]]): Unit =
    xs.foldLeft(Foo.empty)((ys, x) =>
      ys.combine(x) // error
    )
