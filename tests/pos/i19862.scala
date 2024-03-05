import scala.language.implicitConversions

object Test:
  implicit inline def uhOh[A](value: A): A =
    compiletime.error("Should not have been called")
  def test =
    // Compiles because `uhOh` fails to eta-expand and we fallback to `Predef.$conforms[A, A]`
    summon[Function1[Int, Int]]
