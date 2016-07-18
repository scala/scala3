class Foo[A]

trait Bar[DD[_,_]] {
  val x: DD[Int, Int]
}

trait Baz extends Bar[[X,Y] -> Foo[X]] {
  def foo[M[_,_]](x: M[Int, Int]) = x

  foo(x)
}
