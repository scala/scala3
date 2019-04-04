// Test that flow-sensitive type inference handles
// early exists from blocks.
object Test {
  def main(args: Array[String]): Unit = {
    check("hello")
    check("world")
    check2("blocks")
    try {
      check(null)
    } catch {
      case npe: NullPointerException =>
        println("npe")
    }
  }
  
  def err(msg: String) = throw new NullPointerException(msg)

  def check(s: String|Null): String = {
    if (s == null) err("null argument!")
    s
  }

  // Test that flow info is propagated to vals, but not to defs.
  def check2(s: String|Null): String = {
    if (s == null) err("null argument")
    val s2 = s
    def s3 = s.nn // need the "nn"
    s2 ++ s3
  }
}
