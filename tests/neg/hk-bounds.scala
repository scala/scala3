class Foo[A]
class Bar[B]
class Baz[C] extends Bar[C]

object Test1 {
  type Alias[F[X] <: Foo[X]] = F[Int]

  val x: Alias[Bar] = new Bar[Int] // error: Type argument [X0] -> Bar[X0] does not conform to upper bound [X0] -> Foo[X0]

  def foo[F[X] <: Foo[X]] = ()
  foo[Bar] // error: Type argument [X0] -> Bar[X0] does not conform to upper bound [X0] -> Foo[X0]

  def bar[B[X] >: Bar[X]] = ()
  bar[Bar] // ok
  bar[Baz] // // error: Type argument [X0] -> Baz[X0] does not conform to lower bound [X0] -> Bar[X0]
  bar[Foo] // error: Type argument [X0] -> Foo[X0] does not conform to lower bound [X0] -> Bar[X0]

  def baz[B[X] >: Baz[X]] = ()
  baz[Bar] //ok
  baz[Baz] //ok
  baz[Foo] // error: Type argument [X0] -> Foo[X0] does not conform to lower bound [X0] -> Baz[X0]

}
