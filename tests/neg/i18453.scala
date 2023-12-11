// Would be nice if this compiled
// but it doesn't
// because of how we constrain `A`
// and then try to "minimise" its instantiation
trait Box[T]

class Test:
  def f[A, B](c: A => A & B)(using ba: Box[A]): Unit = ???

  def g[X, Y](using bx: Box[X]): Unit =
    def d(t: X): X & Y = t.asInstanceOf[X & Y]
    f(d) // error
