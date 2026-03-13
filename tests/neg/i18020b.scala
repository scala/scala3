// problems with colon fusion, a harder challenge than cold fusion
class i18020(a_: Int): // error
  def f(b_: Int) = 42 // error
  def g_: Int = 27 // error
  def k =
    val x_: Int = 1 // error
    val y_: Int = 2 // error
    x_ + y_ // error
