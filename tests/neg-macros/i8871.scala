import scala.quoted._
object Macro {
  def impl[A : Type](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe = Type.of[A].seal.asInstanceOf[quoted.Type[_ <: AnyRef]]
    '{ (a: ${tpe}) => ???} // error
  }
}
