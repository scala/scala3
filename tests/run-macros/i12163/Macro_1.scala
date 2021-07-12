import scala.quoted.*
import scala.util.matching.Regex

inline def showLabel(inline label: String): String =
  ${ showLabelExpr('label) }


private def showLabelExpr(label: Expr[String])(using Quotes): Expr[String] = {
  val suggestRegex: Regex = "(Type)([a-zA-Z]+)(Mapping)([a-zA-Z]+)".r
  val docRegex: Regex = "(Test)(Mapping)([a-zA-Z]+)".r
  val simpleRegex: Regex = "([a-zA-Z]+)(Mapping)([a-zA-Z]+)".r

  label.value match {
    case Some(docRegex(doc, _, _)) =>
      Expr(doc)

    case Some(suggestRegex(suggest, suggestType, _, _)) =>
      Expr(suggest)

    case Some(simpleRegex(docType, _, _)) =>
      Expr(docType)

    case Some(value) =>
      Expr(s"No label matched: $value")
      // quotes.reflect.report.throwError(s"No label matched: $value")

    case None =>
      Expr(s"Expected label to be a know string but was: ${label.show}")
      // quotes.reflect.report.throwError(s"Expected label to be a know string but was: ${label.show}")
  }
}
