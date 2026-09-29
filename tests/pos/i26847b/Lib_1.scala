import scala.util.NotGiven

class C
class D

object Lib:
  def f(using c: C, d: D)(x: Int): Int = x

  given C = C()
  given D = D()

  inline def g(x: Int): Int = f(x)

// An overloaded variant in the style of specs2's Debug.pp: a two-parameter
// using clause on the extension, followed by unary explicit sections.
trait NoDebug
trait Output:
  def println(m: Any): Unit

object Debug:
  extension [T](t: => T)(using not: NotGiven[NoDebug], output: Output)
    def pp: T = t
    def pp(condition: Boolean): T = t
    def pp(pre: String): T = t

  given Output = new Output:
    def println(m: Any): Unit = ()

  inline def use[T](t: => T): T = t.pp(condition = true)
