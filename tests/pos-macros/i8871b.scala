import scala.quoted._
object Macro {
  def impl[A : Type](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe/*: quoted.QuotedType */ = typeOf[A].seal
    '{ f[$tpe] }
  }
  def f[T <: AnyKind]: Unit = ()
}
