import scala.quoted._
import scala.quoted.matching._

inline def rewrite[T](x: => T): T = ${ rewriteMacro('x) }

def plus(x: Int, y: Int): Int = x + y
def times(x: Int, y: Int): Int = x * y
def power(x: Int, y: Int): Int = if y == 0 then 1 else times(x, power(x, y - 1))

private def rewriteMacro[T: Type](x: Expr[T])(given QuoteContext): Expr[T] = {
  val rewriter = Rewriter(
    postTransform = List(
      Transformation[Int] {
        case '{ plus($x, $y) } =>
          (x, y) match {
            case (Const(0), _) => y
            case (Const(a), Const(b)) => Expr(a + b)
            case (_, Const(_)) =>  '{ $y + $x }
            case _ => '{ $x + $y }
          }
        case '{ times($x, $y) } =>
          (x, y) match {
            case (Const(0), _) => '{0}
            case (Const(1), _) => y
            case (Const(a), Const(b)) => Expr(a * b)
            case (_, Const(_)) => '{ $y * $x }
            case _ => '{ $x * $y }
          }
        case '{ power(${Const(x)}, ${Const(y)}) } =>
          Expr(power(x, y))
        case '{ power($x, ${Const(y)}) } =>
          if y == 0 then '{1}
          else '{ times($x, power($x, ${Expr(y-1)})) }
      }),
    fixPoint = true
  )

  val x2 = rewriter.rewrite(x)

  '{
    println(${Expr(x.show)})
    println(${Expr(x2.show)})
    println()
    $x2
  }
}

object Transformation {
  def apply[T: Type](transform: PartialFunction[Expr[T], Expr[T]]) =
    new Transformation(transform)
}
class Transformation[T: Type](transform: PartialFunction[Expr[T], Expr[T]]) {
  def apply[U: Type](e: Expr[U])(given QuoteContext): Expr[U] = {
    e match {
      case '{ $e: T } => transform.applyOrElse(e, identity) match { case '{ $e2: U } => e2 }
      case e => e
    }
  }
}

private object Rewriter {
  def apply(preTransform: List[Transformation[_]] = Nil, postTransform: List[Transformation[_]] = Nil, fixPoint: Boolean = false): Rewriter =
    new Rewriter(preTransform, postTransform, fixPoint)
}

private class Rewriter(preTransform: List[Transformation[_]] = Nil, postTransform: List[Transformation[_]] = Nil, fixPoint: Boolean) {
  def rewrite[T](e: Expr[T])(given QuoteContext, Type[T]): Expr[T] = {
    val e2 = preTransform.foldLeft(e)((ei, transform) => transform(ei))
    val e3 = rewriteChildren(e2)
    val e4 = postTransform.foldLeft(e3)((ei, transform) => transform(ei))
    if fixPoint && e4 != e then rewrite(e4)
    else e4
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


