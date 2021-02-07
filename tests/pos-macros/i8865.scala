import scala.quoted._
object Macro {

  def impl[A : Type](using Quotes): Expr[A] = {
    import quotes.reflect._
    TypeRepr.of[A].asType match
      case '[tpe] => '{ (a: tpe) => ???}
    '{???}
  }
}