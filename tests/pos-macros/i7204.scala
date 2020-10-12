import scala.quoted._

object Foo {
  def impl(using qctx: QuoteContext) : Unit = {
    import qctx.reflect._
    val Select(_, _) = (??? : Term)
  }
}
