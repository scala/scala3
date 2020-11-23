import scala.quoted._

object Foo {
  def impl(using Quotes) : Unit = {
    import qctx.reflect._
    val Select(_, _) = (??? : Term)
  }
}
