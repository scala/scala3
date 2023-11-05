class Ops[sealed A](xs: Array[A]) extends AnyVal:

  def f(p: A => Boolean): Array[A] = xs
