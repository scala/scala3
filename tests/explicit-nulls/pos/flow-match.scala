// Test flow-typing when NotNullInfos are from cases

object MatchTest {

  def lengthOfStringOrNull(s: String | Null): Int = s match {
    case _: String => s.length
    case _ => 0
  }

  def stringOrNullToString(s: String | Null): String = s match {
    case null => "null"
    // after the null case, s becomes non-nullable
    case _ => s
  }
}
