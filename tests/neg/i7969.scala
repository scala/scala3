object O{
  def f(i: => Int): Int = 1
  def m[A](a: A => Int) = 2
  def n = m(f)  // error: cannot eta expand
}