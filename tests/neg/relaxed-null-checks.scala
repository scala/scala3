import scala.language.strictEquality
import scala.language.experimental.relaxedNullChecks

// The other operand's type must be a supertype of Null
def values(x: Int) =
  val _ = x == null // error
  val _ = null != x // error
  x match
    case null => // error
    case _ =>

def generic[A](x: A) =
  val _ = x == null // error
  val _ = null != x // error
  x match
    case null => // error
    case _ =>

// Only the `null` literal is special-cased, not other expressions of type Null
def nullTyped(x: Int | Null, n: Null) =
  val _ = x == n // error
  val _ = n != x // error
