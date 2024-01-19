

class Test {
  trait A[+T]
  class B[T] extends A[T]

  class C
  class D extends C

  def quux(a: A[C]): Unit = a match {
    case _: B[C] => // warn
  }

  quux(new B[D])
}
