// Check that selecting a member from a nullable union is unsound.
// Enabling unsafeNulls allows this kind of unsafe operations,
// but could cause exception during runtime.

import scala.language.unsafeNulls

object Test {
  def main(args: Array[String]): Unit = {
    val s: String | Null = "hello"
    assert(s.length == 5)

    val s2: String | Null = null
    try {
      s2.length // should throw
      assert(false)
    } catch {
      case e: NullPointerException =>
        // ok: Selecting on a null value would throw NullPointerException.
    }
  }
}
