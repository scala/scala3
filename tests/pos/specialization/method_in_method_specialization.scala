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
}