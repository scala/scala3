//> using options -experimental
package unroll

class Unrolled{
  def foo(s: String)(f: String => String) = f(s)
}
