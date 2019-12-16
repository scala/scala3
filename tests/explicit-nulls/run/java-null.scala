// Check that selecting a member from a `UncheckedNull`able union is unsound.

object Test {
  def main(args: Array[String]): Unit = {
    val s: String|UncheckedNull = "hello"
    assert(s.length == 5)

    val s2: String|UncheckedNull = null
    try {
      s2.length // should throw
      assert(false)
    } catch {
      case e: NullPointerException =>
        // ok: selecting on a UncheckedNull can throw
    }
  }
}
