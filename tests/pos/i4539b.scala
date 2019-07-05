import scala.quoted._
def test given QuoteContext = {
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

  def bar[T](t: quoted.Type[T]) = ???
  bar('[String])

  class Baz[T](t: quoted.Type[T])
  new Baz('[String])

}
