import scala.quoted.*
def test(using Quotes): Unit = {
  def f = {
    {
      Type.of[String]
      Type.of[String]
    }

    Type.of[String] match { case _ => }
    try Type.of[String] catch { case _ => }

    Type.of[String]
    Type.of[String]
  }

  def bar[T](t: Type[T]) = ???
  bar(Type.of[String])

  class Baz[T](t: Type[T])
  new Baz(Type.of[String])

}
