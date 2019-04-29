
trait T {
  def i: Int = 123_456_ // error
  def j: Long = 123_456_L * 1000 // error

  def f = 3_14_E-2 // error
  def e = 3_14E-_2 // error
  def d = 3_14E-2_ // error

  def p = 3.1_4_ // error
  def q = 3.1_4_d // error
  def r = 3.1_4_dd // error // error
  def s = 3_.1 // error

  def z = 0
}