
sealed trait O
object A extends O
object B extends O

object Test {

  def test(x: O) =
  (x, x, x, x, x, x, x, x) match {
    case (A, A, A, A, A, A, A, A) => 1
    case (B, B, B, B, B, B, B, B) => 2
    case (_, A, A, A, A, A, A, A) => 3
    case (_, B, B, B, B, B, B, B) => 4
    case (_, _, A, A, A, A, A, A) => 5
    case (_, _, B, B, B, B, B, B) => 6
    case (_, _, _, A, A, A, A, A) => 7
    case (_, _, _, B, B, B, B, B) => 8
    case (_, _, _, _, A, A, A, A) => 9
    case (_, _, _, _, B, B, B, B) => 10
    case (_, _, _, _, _, A, A, A) => 11
    case (_, _, _, _, _, B, B, B) => 12
    case (_, _, _, _, _, _, A, A) => 13
    case (_, _, _, _, _, _, B, B) => 14
    case (_, _, _, _, _, _, _, A) => 15
    case (_, _, _, _, _, _, _, B) => 16

  }
}
