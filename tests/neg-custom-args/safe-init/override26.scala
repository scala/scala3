abstract class A {
  val h: () => Int
}

class B extends A {
  val h = () => f
  def f: Int = b
  val b = 10

  new C(this)     // error
}

class C(a: Warm[A]) { // Warm has no effect, as objects only escape as Cold
  val n = a.h()
}