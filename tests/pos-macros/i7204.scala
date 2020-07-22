import scala.quoted._

object Foo {
  def impl(using qctx: QuoteContext) : Unit = {
    import qctx.tasty._
    val Select(_, _) = (??? : Term)
  }
}
