import scala.quoted._
object Macro {
  def impl[A : Staged](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe/*: quoted.Type */ = typeOf[A].seal
    '{ f[$tpe] }
  }
  def f[T <: AnyKind]: Unit = ()
}
