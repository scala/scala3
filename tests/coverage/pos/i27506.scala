package covtest

object A:
  object Inner:
    def f(x: Int) = x + 1

object B:
  object Inner:
    def f(x: Int) = x + 2

class Outer:
  trait Middle:
    object Inner:
      def f(x: Int) = x + 3

object Methods:
  def f(x: Int) =
    class Inner:
      def g(y: Int) = y + 4
    Inner().g(x)
