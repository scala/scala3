object Fluent {
  trait Foo[C[_]] {
    def meth1[T]() : C[T]
  }
  trait CC[T]

  type Context[Alg[x[_]] <: Foo[x], E] = given Alg[CC] => CC[E]

  def meth1[T]() : Context[Foo, T] = {
    implicitly[Foo[CC]].meth1()
  }
}
