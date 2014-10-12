// Encoding with type fields of the
// variance test in pos. This time, the
// test does not pass because of the encoding
// This points to a difference between type members
// and parameters which we should investigate further.
object Test2 {

  trait A {
    type X
    protected[this] def f(x: X): X = x
  }

  trait B extends A {
    type X <: B
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
  class C extends B with A {
    type X <: C
    override protected[this] def f(c: C) = c
  }

// java.lang.ClassCastException: B$$anon$1 cannot be cast to C
//  at C.f(<console>:15)
  new C().kaboom
}
