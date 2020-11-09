import scala.quoted._
object Macro {
  def impl[A : Type](using qctx: QuoteContext): Unit = {
    import qctx.reflect._
    val tpe/*: Type[? <: AnyKind]*/ = TypeRepr.of[A].asType
    '{ f[$tpe] } // error
  }
  def f[T <: AnyKind]: Unit = ()
}
