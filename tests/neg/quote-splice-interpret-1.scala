
import scala.quoted._

object Macros {
  rewrite def isZero(transparent n: Int): Boolean = ~{ // error
    if (n == 0) '(true)
    else '(false)
  }
}
