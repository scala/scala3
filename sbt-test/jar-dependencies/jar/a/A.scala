package example:
  trait A:
    def foo: String = ???

package object example extends A:
  def foo(x: String): String = ???
