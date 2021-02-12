import scala.quoted.*
object Macro {

  def impl[A : Type](using Quotes): Expr[A] = {
    import quotes.reflect.*
    TypeRepr.of[A].asType match
      case '[tpe] => '{ (a: tpe) => ???}
    '{???}
  }
}