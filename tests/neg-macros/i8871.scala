import scala.quoted._
object Macro {
  def impl[A : Type](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe = typeOf[A].asQuotedType.asInstanceOf[quoted.Type[_ <: AnyRef]]
    '{ (a: ${tpe}) => ???} // error
  }
}
