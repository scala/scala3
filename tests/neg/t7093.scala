trait A[+X] {
  protected[this] def f(x: X): X = x // error: variance
}

trait B extends A[B] {
  def kaboom = f(new B {})
}

// protected[this] disables variance checking
// of the signature of `f`.
//
// C's parent list unifies A[B] with A[C]
//
// The protected[this] loophole is widely used
// in the collections, every newBuilder method
// would fail variance checking otherwise.
class C extends B with A[C] {
  override protected[this] def f(c: C) = c
}

// java.lang.ClassCastException: B$$anon$1 cannot be cast to C
//  at C.f(<console>:15)
object Test extends App {
  new C().kaboom
}
