// This test case is intended to verify that reduce projection
// works across multiple refinements. When running with -Xprint:front
// The inferred type of y and yy should be String.
// It would be good to improve our testing framework so
// that we can verify this. With partest, it would be easy.
class ABC { type A; type B; type C }

object Test {

  val x: (ABC { type C = String; type B = C; type A = B }) # A = ???

  val y = x  // should expand to: val y: String = x

  val xx: (ABC { type C = String } { type B = C } { type A = B }) # A = ???

  val yy = x  // should expand to: val y: String = x

}
