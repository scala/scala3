trait Foo[V]
class Inv[A](a: A)

trait Bar[K <: Foo[V], V] {
  // ERROR: Type argument Foo[Inv[V]] does not conform to upper bound Foo[Inv[Inv[V]]]
  def test(): Bar[Foo[Inv[V]], Inv[V]]
}