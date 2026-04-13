object test {
  class A
  class B extends A

  inline def f[T](inline v: Any): T = inline v match {
    case x: T => x
  }

  inline def b: B = new B()
  f[A](b: A)
}
