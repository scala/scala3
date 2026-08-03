object AstVisitor:
  def visit(o: Option[String]): String =
    sangria.Macros.useOrNullFromTransparentInline(o)
    sangria.Macros.useOrNullFromInline(o)
    sangria.Macros.useOrNullFromMacro(o)
