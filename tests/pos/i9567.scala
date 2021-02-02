// object Test {
//   val x: Int => Int = identity
// }

trait Foo[F[_], I[X] <: X] {
  type Id[X] <: X
  def foo[G[x] >: F[x]]: G[Unit]
  def foo2[X >: F[String]]: Id[X]
  def foo3[X >: F[String]]: I[X]
}

trait M[A] {
  def bla: Int = 1
  def baz(f: Int => Int): Int = f(1)
}

object Test {
  def bar(x: Foo[M, [X] =>> X]): Unit = {
    x.foo.bla
    x.foo.baz(x => x)
    x.foo2.bla
    x.foo3.bla
  }
}
