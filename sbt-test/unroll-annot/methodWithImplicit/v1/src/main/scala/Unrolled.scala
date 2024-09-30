package unroll

class Unrolled{
  def foo(s: String)(implicit f: String => String) = f(s)
}
