import language.postfixOps
trait T {
  def f = 3_14_E-2 // error
  def e = 3_14E-_2 // error
  def d = 3_14E-2_ // error

  def p = 3.1_4_ // error
  def q = 3.1_4_d // error
  def r = 3.1_4_dd // error // error
  def s = 3_.1 // error

  def tooSmall = 1.0E-325 // error

  // Examples from
  // https://docs.oracle.com/javase/8/docs/technotes/guides/language/underscores-literals.html

  val pi1 = 3_.1415F // error
  val pi2 = 3._1415F // error
  val socialSecurityNumber1
    = 999_99_9999_L // error
  val x1 = _52 // error
  val x3 = 52_ // error

  val x5 = 0_x52 // error // error
  val x6 = 0x_52
  val x8 = 0x52_ // error
  val x9 = 0_52
  val x10 = 052
  val x11 = 0_0.52
  val x12 = 00.52
  val x13 = 00
  val x14 = 00d
  val x15 = 00.0
  val x16 = 0_0

  def z = 0
}
