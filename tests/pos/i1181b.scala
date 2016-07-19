class Foo[A]

object Test {
  def foo[M[_,_]](x: M[Int,Int]) = x

  type Alias[X,Y] = Foo[X]
  val x: Alias[Int,Int] = new Foo[Int]

  foo[Alias](x) // ok
  foo(x)
}
