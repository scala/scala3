package covtest

object MatchNumbers:
  type Integer = Int | Long

  def f(i: Integer): Int = i match
    case x: Int if x < 0 => -1
    case x: Int => x
    case y: Long => y.toInt

  f(0)
  f(1L)
