object Test {
  sealed trait EQ[A, B]
  final case class Refl[T]() extends EQ[T, T]

  def absurd[F[_], X, Y](eq: EQ[F[X], F[Y]], x: X): Y = eq match {
    case Refl() => x
  }

  var ex: Exception = _
  try {
    type Unsoundness[X] = Int
    val s: String = absurd[Unsoundness, Int, String](Refl(), 0)
  } catch {
    case e: ClassCastException => ex = e
  }

  def main(args: Array[String]) =
    assert(ex != null)
}
