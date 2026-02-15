import scala.language.strictEquality
val x: Int | Null = 42
val _ = x == null
val _ = null == x
val _ = x != null
val _ = null != x
val _ = x match
  case null => 
  case _: Int =>
