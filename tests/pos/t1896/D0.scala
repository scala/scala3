package p

class X[T]

trait A {
  def m(s:X[?]): Unit = {}
}

trait B extends A {
  def f: Unit = { super.m(null) }
}
