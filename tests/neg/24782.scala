object S {
  trait F[A]
  type Pure = [a] => a => F[a]
  def f(using pure: Pure) = S(s => pure(s)) // error (but no compiler crash / stack overflow)
}
