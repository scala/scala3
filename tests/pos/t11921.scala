object t1:
  class C
  class A:
    val c: B.c.type = B.c
  object B:
    val c = new C
    val a = new A:
      def m = c    // not ambiguous, `this.c` and `B.c` are compatible paths

object t2:
  class C[T]:
    type TT = T
  object O:
    type TT = String
    class D extends C[TT]:
      def n(x: TT) = x  // `TT` is not ambiguous, `this.TT` and `O.TT` are aliases
