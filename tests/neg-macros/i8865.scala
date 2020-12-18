import scala.quoted._
object Macro {

  def impl[A : Type](using Quotes): Expr[A] = {
    import quotes.reflect._
    val tpe = TypeRepr.of[A].asType
    '{ (a: ${tpe}) => ???} // error: tpe.Underlying cannot be used as a value type
    '{???}
  }
}