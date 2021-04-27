// Test flow-typing when NotNullInfos are from non-null cases

object MatchTest {
  locally {
    val s: String|Null = ???
    s match {
      case _: String => println(s.length)
      case _ => println(0)
    }
  }
}
