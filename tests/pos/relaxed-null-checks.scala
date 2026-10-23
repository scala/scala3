import scala.language.strictEquality
import scala.language.experimental.relaxedNullChecks

def values(x: Int | Null) =
  val _ = x == null
  val _ = null == x
  val _ = x != null
  val _ = null != x
  x match
    case null =>
    case _: Int =>

def generic[A](x: A | Null) =
  val _ = x == null
  val _ = null != x
  x match
    case null =>
    case _ =>

def bounded[A >: Null](x: A) =
  val _ = x == null
  val _ = null != x
  x match
    case null =>
    case _ =>
