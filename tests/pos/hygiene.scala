// Illustrates a failure with hygiene.

object hygiene {

  class D[T]

  case class C[T](x: D[T])
// gives
// 7: error: wrong number of type arguments for hygiene.C.D, should be 0
// 7: error: constructor C in class C does not take type parameters
//
// The second error message is straightforward to fix using a refTypeTree for C in
// desugar.classDef.classTypeRef, but the first one is much harder.


  object C {

    class C

//    class D

  }

  val c = C.apply(new D)

  c.x

}
