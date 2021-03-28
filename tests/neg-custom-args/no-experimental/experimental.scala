class Test0 {
  import language.experimental.namedTypeArguments // error
  object Foo {
    inline def f[S, T](x: S): T = ???
    def g(x: Int) = f[T = Any](x)                 // error
  }
}

class Test1 {
  import language.experimental.erasedDefinitions  // error
  import scala.compiletime.erasedValue
  type UnivEq[A]
  object UnivEq:
    erased def force[A]: UnivEq[A] = erasedValue  // error // error // error
    extension [A](erased proof: UnivEq[A])        // error
      inline def univEq(a: A, b: A): Boolean =
        a == b
}

class Test1 {
  import language.experimental.genericNumberLiterals // error
  val x: BigInt = 13232202002020202020202
  val y: BigInt = -0xaabb12345ACF12345AC
}

