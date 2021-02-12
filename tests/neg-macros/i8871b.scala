import scala.quoted.*
object Macro {
  def impl[A : Type](using Quotes) = {
    import quotes.reflect.*
    val tpe/*: Type[? <: AnyKind]*/ = TypeRepr.of[A].asType
    '{ f[$tpe] } // error
  }
  def f[T <: AnyKind]: Unit = ()
}
