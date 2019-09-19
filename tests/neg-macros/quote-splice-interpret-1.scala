
import scala.quoted.{_, given}

object Macros {
  inline def isZero(inline n: Int): Boolean = ${
    if (n == 0) 'true // error
    else 'false
  }
}
