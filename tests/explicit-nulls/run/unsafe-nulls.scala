// Check that selecting a member from a nullable union is unsound.
// Enabling unsafeNulls allows this kind of unsafe operations,
// but could cause exception during runtime.

object F {
  def apply(x: String): String = x
}

object G {
  def h(f: String | Null => String, x: String | Null): String | Null =
    f(x)
}

object Test {
  import scala.language.unsafeNulls

  def main(args: Array[String]): Unit = {
    val s1: String | Null = "hello"
    assert(s1.length == 5)

    val s2: String | Null = null
    try {
      s2.length // should throw
      assert(false)
    } catch {
      case e: NullPointerException =>
        // ok: Selecting on a null value would throw NullPointerException.
    }

    val s3: String = F(s1)
    assert(s3.length == 5)

    val s4: String = G.h(F.apply, s1)
    assert(s4.length == 5)
  }
}
