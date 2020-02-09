import scala.compiletime.ops._
import scala.annotation.infix

object Test {
  @infix type +[+X <: Int & Singleton | String & Singleton, +Y <: Int & Singleton | String & Singleton] = (X, Y) match {
    case (Int & Singleton, Int & Singleton) => int.+[X, Y]
    case (String & Singleton, String & Singleton) => string.+[X, Y]
    case (String & Singleton, Int & Singleton) => string.+[X, int.ToString[Y]]
    case (Int & Singleton, String & Singleton) => string.+[int.ToString[X], Y]
  }

  val t0: "a" + 1 = "a1"
  val t1: "a" + "b" = "ab"
  val t2: 1 + "b" = "1b"
  val t3: 1 + 1 = 2
}
