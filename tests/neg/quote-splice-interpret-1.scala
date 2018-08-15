
import scala.quoted._

object Macros {
  transparent def isZero(n: Int & Constant): Boolean = ~{ // error
    if (n == 0) '(true)
    else '(false)
  }
}
