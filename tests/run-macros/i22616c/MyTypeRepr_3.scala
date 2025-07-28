import scala.quoted.*

final class MyTypeRepr(using val quotes: Quotes)(val unwrap: quotes.reflect.TypeRepr) {
  import quotes.reflect.*

  def getAnnotation(annotTpe: quotes.reflect.Symbol): Option[quotes.reflect.Term] =
    unwrap.typeSymbol.getAnnotation(annotTpe)

  def optionalAnnotation[Annot: Type]: Option[Expr[Annot]] = {
    val annotTpe = TypeRepr.of[Annot]
    val annotFlags = annotTpe.typeSymbol.flags

    if (annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait))
      report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")

    this.getAnnotation(annotTpe.typeSymbol) match
      case Some(tree) if tree.tpe <:< annotTpe => Some(tree.asExprOf[Annot])
      case _                                   => None
  }

  def requiredAnnotation[Annot: Type]: Expr[Annot] =
    optionalAnnotation[Annot].getOrElse(report.errorAndAbort(s"Missing required annotation `${TypeRepr.of[Annot].show}` for `$this`"))

  def optionalAnnotationValue[Annot: {Type, FromExpr}]: Option[Annot] =
    optionalAnnotation[Annot].map { expr =>
      expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$this`, but are unable to extract Expr.value\n${expr.show}"))
    }

  def requiredAnnotationValue[Annot: {Type, FromExpr}]: Annot = {
    val expr = requiredAnnotation[Annot]
    expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$this`, but are unable to extract Expr.value\n${expr.show}"))
  }
}
