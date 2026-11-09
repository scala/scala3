import language.experimental.safe
import A.*

object Test:
  val _ = f(0)  // error
  val _ = g(0)
  val _ = f2(0) // error
  val _ = g2(0)
