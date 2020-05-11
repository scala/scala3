import scala.quoted._
def test(using s: Scope) = {
  def f = {
    {
      '[String]
      '[String]
    }

    '[String] match { case _ => }
    try '[String] catch { case _ => }

    '[String]
    '[String]
  }

  def bar[T](t: s.Type[T]) = ???
  bar('[String])

  class Baz[T](t: s.Type[T])
  new Baz('[String])

}
