object ImplicitScope {
  class A[T]

  def foo: Unit = {
    trait B
    object B {
      implicit def ab: ImplicitScope.A[B] = new A[B]
    }

    implicitly[A[B]]  // Error
  }
}
