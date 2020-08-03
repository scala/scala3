import scala.quoted._
object Macro {
  def impl[A : Staged](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe = typeOf[A].seal.asInstanceOf[quoted.Staged[_ <: AnyRef]]
    '{ (a: ${tpe}) => ???}
  }
}
