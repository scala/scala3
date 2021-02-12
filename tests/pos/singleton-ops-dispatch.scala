import scala.compiletime.ops.*

object Test {
  infix type +[X <: Int | String, Y <: Int | String] = (X, Y) match {
    case (Int, Int) => int.+[X, Y]
    case (String, String) => string.+[X, Y]
    case (String, Int) => string.+[X, int.ToString[Y]]
    case (Int, String) => string.+[int.ToString[X], Y]
  }

  val t0: "a" + 1 = "a1"
  val t1: "a" + "b" = "ab"
  val t2: 1 + "b" = "1b"
  val t3: 1 + 1 = 2
}
