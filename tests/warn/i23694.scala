//> using options -Wunused:privates

trait A:
  def get: Any = new A.C // select has type [T] =>> C[T]
  def cc: Any = new A.CC // select has type CC

object A:
  private class C[T]
  private type CC[T] = C[T]

trait B:
  def get: Any = new B.C

object B:
  private class C

// duplicate issue #23960
package net.marek.tyre.automaton:

  private[tyre] class TyreCompiler[IN <: Tuple, R]:
    private def compile[IS <: Tuple, T](xs: List[T]): String = xs match // warn
      case _: List[t] =>
        Loop[IS, t](null.asInstanceOf[t]).build
    private class Loop[IS <: Tuple, T](
      val i: T,
    ):
      def build = "27"
