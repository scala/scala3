import scala.quoted._
object Macro {
  def impl[A : Type](using qctx: QuoteContext): Unit = {
    import qctx.tasty._
    val tpe/*: quoted.Type[? <: AnyKind]*/ = Type.of[A].seal
    '{ f[$tpe] } // error
  }
  def f[T <: AnyKind]: Unit = ()
}
