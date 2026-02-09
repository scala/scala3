// Test unreachable matches in presence of nulls

object MatchTest2 {
  def f6(s: String | Null): String = s match {
    case s2 => "string"
    case null => "other" // warn
    case s3 => s3 // warn
  }

  def f7(s: String | Null): String = s match {
    case null => "other"
    case null => "other" // warn
    case s3: String => s3
    case s4 => s4 // warn
  }
}