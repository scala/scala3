import scala.quoted._
def test(using QuoteContext) = {
  def f = {
    {
      Type[String]
      Type[String]
    }

    Type[String] match { case _ => }
    try Type[String] catch { case _ => }

    Type[String]
    Type[String]
  }

  def bar[T](t: quoted.Type[T]) = ???
  bar(Type[String])

  class Baz[T](t: quoted.Type[T])
  new Baz(Type[String])

}
