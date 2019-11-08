// Check that selecting a member from a `JavaNull`able union is unsound.

object Test {
  def main(args: Array[String]): Unit = {
    val s: String|JavaNull = "hello"
    assert(s.length == 5)

    val s2: String|JavaNull = null
    try {
      s2.length // should throw
      assert(false)
    } catch {
      case e: NullPointerException =>
        // ok: selecting on a JavaNull can throw
    }
  }
}
