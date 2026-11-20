import scala.quoted.*

object X:
  def asd(using q: Quotes)(f: [A] => Type[A] ?=> q.reflect.TypeRepr): Unit =
    f[Int]

  def test1(using Quotes) =
    import quotes.reflect.*
    asd([A] => TypeRepr.of[A])

  def test2(using Quotes) =
    import quotes.reflect.*
    asd([A] => (_: Type[A]) ?=> TypeRepr.of[A])
