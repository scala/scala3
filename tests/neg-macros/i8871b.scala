import scala.quoted._
object Macro {
  def impl[A : Type](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe/*: quoted.Type[? <: AnyKind]*/ = typeOf[A].asQuotedType
    '{ f[$tpe] } // error
  }
  def f[T <: AnyKind]: Unit = ()
}
