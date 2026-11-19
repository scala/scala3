// scalajs: --skip
// (uses reflection)

class C extends Function1[Int, Int]:
  override def apply(v1: Int): Int =
    v1 * 2

object Test:
  def main(args: Array[String]): Unit =
    val c = new C()
    // Ensure it works with a direct call (which may be rewritten to the specialized form),
    println(c.apply(21))
    // as well as with specific calls to the unspecialized and specialized forms
    println(c.getClass.getDeclaredMethod("apply", classOf[Int]).invoke(c, 21))
    println(c.getClass.getDeclaredMethod("apply$mcII$sp", classOf[Int]).invoke(c, 21))
