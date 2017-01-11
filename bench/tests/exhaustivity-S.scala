
sealed trait O
object A extends O
object B extends O

object Test {

  def test(x: O) =
  (x, x, x, x, x, x, x, x, x, x, x, x, x, x) match {
    case (A, A, _, _, _, _, _, _, _, _, _, _, _, _) => 1
    case (_, _, A, A, _, _, _, _, _, _, _, _, _, _) => 2
    case (_, _, _, _, A, A, _, _, _, _, _, _, _, _) => 3
    case (_, _, _, _, _, _, A, A, _, _, _, _, _, _) => 4
    case (_, _, _, _, _, _, _, _, A, A, _, _, _, _) => 5
    case (_, _, _, _, _, _, _, _, _, _, A, A, _, _) => 6
    case (_, _, _, _, _, _, _, _, _, _, _, _, A, A) => 7
    case (B, A, B, A, B, A, B, A, B, A, B, A, B, A) => 8

  }
}
