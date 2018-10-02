class I0 {
  class I1
  def test0 = {
    val x = new y.I1 // error: `y` is a forward reference extending over the definition of `x`
    val y = new I0
  }

  def test1 = {
    type T = y.I1
    val x = new T // error: `y` is a forward reference extending over the definition of `x`
    val y = new I0
  }

  class I2[T1, T2]
  def test2 = {
    type A[T] = y.I2[T, String]
    val x = new A[Int] // error: `y` is a forward reference extending over the definition of `x`
    val y = new I0
  }

  def test3 = {
    val x = new T // error: `T` is a forward reference extending over the definition of `x`
    val y = new I0
    type T = y.I1
  }
}
