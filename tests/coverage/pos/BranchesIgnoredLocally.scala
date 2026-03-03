package covtest

class BranchesIgnoredLocally:
  def coveredBranch(x: Int): String =
    if x > 0 then "positive"
    else "non-positive"

  // $COVERAGE-OFF$
  def ignoredBranch(x: Int): String =
    if x > 0 then "positive"
    else "non-positive"

  def ignoredMatch(x: Int): String = x match
    case 1 => "one"
    case 2 => "two"
    case _ => "other"

  def ignoredTryCatch: Int =
    try 1 / 0
    catch
      case _: ArithmeticException => -1
  // $COVERAGE-ON$

  def coveredMatch(x: Any): String = x match
    case _: Int => "int"
    case _: String => "string"
    case _ => "other"
