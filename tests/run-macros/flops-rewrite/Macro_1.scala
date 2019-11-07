import scala.quoted._

inline def rewrite[T](x: => T): T = ${ rewriteMacro('x) }

private def rewriteMacro[T: Type](x: Expr[T])(given QuoteContext): Expr[T] = {
  val rewriter = Rewriter(
    postTransform = {
      case '{ Nil.map[$t]($f) } => '{ Nil }
      case '{ Nil.filter($f) }  => '{ Nil }
      case '{ Nil.++[$t]($xs) } => xs
      case '{ ($xs: List[$t]).++(Nil) } => xs
      case x => x
    }
  )

  val x2 = rewriter.rewrite(x)

  '{
    println(${Expr(x.show)})
    println(${Expr(x2.show)})
    println()
    $x2
  }
}

private object Rewriter {
  def apply(preTransform: Expr[Any] => Expr[Any] = identity, postTransform: Expr[Any] => Expr[Any] = identity, fixPoint: Boolean = false): Rewriter =
    new Rewriter(preTransform, postTransform, fixPoint)
}

private class Rewriter(preTransform: Expr[Any] => Expr[Any], postTransform: Expr[Any] => Expr[Any], fixPoint: Boolean) {
  def rewrite[T](e: Expr[T])(given QuoteContext, Type[T]): Expr[T] = {
    val e2 = checkedTransform(e, preTransform)
    val e3 = rewriteChildren(e2)
    val e4 = checkedTransform(e3, postTransform)
    if fixPoint && e4 != e then rewrite(e4)
    else e4
  }

  private def checkedTransform[T: Type](e: Expr[T], transform: Expr[T] => Expr[Any])(given QuoteContext): Expr[T] = {
    transform(e) match {
      case '{ $x: T } => x
      case '{ $x: $t } => throw new Exception(
        s"""Transformed
           |${e.show}
           |into
           |${x.show}
           |
           |Expected type to be
           |${summon[Type[T]].show}
           |but was
           |${t.show}
         """.stripMargin)
    }
  }

  def rewriteChildren[T: Type](e: Expr[T])(given qctx: QuoteContext): Expr[T] = {
    import qctx.tasty.{_, given}
    class MapChildren extends TreeMap {
      override def transformTerm(tree: Term)(given ctx: Context): Term = tree match {
        case IsClosure(_) =>
          tree
        case IsInlined(_) | IsSelect(_) =>
          transformChildrenTerm(tree)
        case _ =>
          tree.tpe.widen match {
            case IsMethodType(_) | IsPolyType(_) =>
              transformChildrenTerm(tree)
            case _ =>
              tree.seal match {
                case '{ $x: $t } => rewrite(x).unseal
              }
          }
      }
      def transformChildrenTerm(tree: Term)(given ctx: Context): Term =
        super.transformTerm(tree)
    }
    (new MapChildren).transformChildrenTerm(e.unseal).seal.cast[T] // Cast will only fail if this implementation has a bug
  }

}


