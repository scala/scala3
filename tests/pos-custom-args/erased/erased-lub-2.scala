trait Foo

trait PF[A, +B] {
  def apply(x: A): B
}

object Test {
  def orElse2[A1, B1 >: Foo](that: PF[A1, B1]): PF[A1, B1] = ???

  def identity[E]: PF[E, E] = ???

  def foo: PF[Foo, Foo] = ???

  def bla(foo: Foo) = orElse2(identity).apply(foo)
}
