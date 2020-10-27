import scala.quoted._
def test(using QuoteContext): Unit = {
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

  def bar[T](t: Type[T]) = ???
  bar(Type[String])

  class Baz[T](t: Type[T])
  new Baz(Type[String])

}
