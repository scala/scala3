
trait T {
  def i: Int = 1_024
  def j: Long = 1_024L * 1_024
  //def k = 1'024

  def f = 3_14e-2f
  def d = 3_14E-2_1

  def z = 0


  // Examples from
  // https://docs.oracle.com/javase/8/docs/technotes/guides/language/underscores-literals.html
  val x2 = 5_2;              // OK (decimal literal)
  val x4 = 5_______2;        // OK (decimal literal)
  val x7 = 0x5_2;            // OK (hexadecimal literal)
}