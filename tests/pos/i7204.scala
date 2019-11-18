import scala.quoted._

object Foo {
  def impl(given qctx: QuoteContext): Unit = {
    import qctx.tasty.{_, given}
    val Select(_, _) = (??? : Term)
  }
}
