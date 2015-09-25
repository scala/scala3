object method_in_method_specialization {
  def outer[@specialized(Int) O](o: O): O = {
    def inner[@specialized(Int, Char) I](i: I): O = i match {
      case o2: O => o2
      case _     => o
    }
    inner(1)
    inner('c')
  }

  outer(2)
  outer('d')

  def outer2[@specialized(Int) O](o: O): Int = {
    def inner2[@specialized(Int) I] (i: I) = 1
    inner2(42)
  }
  outer2(1)
}