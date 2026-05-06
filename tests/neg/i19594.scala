import scala.annotation.implicitNotFound

@implicitNotFound("Can you see me?!")
trait Compare[-A, -B]

object Compare:
  val any: Compare[Any, Any] = new Compare {}

object example extends App:

  // The presence of the below default argument prevents the `implicitNotFound` message from appearing
  def assertEquals[A, B](a: A, b: B, clue: => Any = "values are not the same")
                        (implicit comp: Compare[A, B]): Unit = ()

  assertEquals(true, 1, "values are not the same") // error
  assertEquals(true, 1) // error

object updated:
  def f[A, B](a: A, b: B)(using Compare[A, B]) = ()
  f(true, 1) // error

  def g[A, B](a: A, b: B, clue: => Any)(implicit comp: Compare[A, B]) = ()
  g(true, 1, "values are not the same") // error

  def h[A, B](a: A, b: B)(using c: Compare[A, B] = Compare.any, s: String) = ()
  h(true, 1) // error

  class X
  val defx = X()
  def competing[A, B](a: A, b: B)(implicit x: X = defx, cmp: Compare[A, B]) = ()
  competing(true, 42) // error when the default is in the same param list as the missing implicit
