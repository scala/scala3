package example:
  def xd: Int = ???

package bar:
  trait A:
    def foo: String = ???

package object example extends bar.A:
  def foo(x: String): String = ???
