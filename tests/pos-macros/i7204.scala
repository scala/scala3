import scala.quoted.*

object Foo {
  def impl(using Quotes) : Unit = {
    import quotes.reflect.*
    val Select(_, _) = (??? : Term)
  }
}
