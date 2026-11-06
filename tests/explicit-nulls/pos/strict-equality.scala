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

def refs(s: String | Null) =
  val _ = s == null
  val _ = null != s
  s match
    case null =>
    case _ =>

// Motivating example of SIP-79: null checks enable flow typing
def flowIf(x: Int | Null): Int =
  if x != null then x else 0

def flowIfNot(x: Int | Null): Int =
  if x == null then 0 else x

def flowMatch(x: Int | Null): Int =
  x match
    case null => 0
    case y => y

def flowGeneric[A](x: A | Null, default: A): A =
  if x != null then x else default
