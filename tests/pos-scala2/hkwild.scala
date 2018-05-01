class Test[T1](val x: T1) {
  def invert[El1, CC1[X]](implicit w1: T1 <:< CC1[El1]) = {
    val buf: CC1[_] = w1(x)
    ???
  }
}
