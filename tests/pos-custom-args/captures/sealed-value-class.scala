class Ops[A](xs: Array[A]^) extends AnyVal:
  this: Ops[A]^ =>
  def f(p: A => Boolean): Array[A]^{this} = xs
