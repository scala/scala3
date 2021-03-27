import language.experimental.erasedDefinitions
import scala.compiletime.erasedValue
type UnivEq[A]
object UnivEq:
  erased def force[A]: UnivEq[A] = erasedValue
  extension [A](erased proof: UnivEq[A])
    inline def univEq(a: A, b: A): Boolean =
      a == b
