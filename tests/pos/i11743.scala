import language.experimental.erasedDefinitions

type UnivEq[A]
object UnivEq:
  erased def force[A]: UnivEq[A]
  extension [A](erased proof: UnivEq[A])
    inline def univEq(a: A, b: A): Boolean =
      a == b
