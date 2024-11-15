//> using options -experimental
package unroll

class Unrolled{
  def foo(f: String => String)(s: String) = f(s)
}
