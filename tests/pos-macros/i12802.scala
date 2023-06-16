import scala.quoted._

object Boo:
  def foo(using Quotes): Unit =
    import quotes.reflect._
    given Option[Symbol] = Some[Symbol](???)
  def bar(using Quotes): Unit =
    import quotes.reflect.Symbol
    given Option[Symbol] = Some[Symbol](???)
