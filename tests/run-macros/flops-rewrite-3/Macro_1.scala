import scala.quoted._
import scala.quoted.matching._

inline def rewrite[T](x: => T): T = ${ rewriteMacro('x) }

def plus(x: Int, y: Int): Int = x + y
def times(x: Int, y: Int): Int = x * y
def power(x: Int, y: Int): Int = if y == 0 then 1 else times(x, power(x, y - 1))

private def rewriteMacro[T: Type](x: Expr[T])(given QuoteContext): Expr[T] = {
  val rewriter = Rewriter().withFixPoint.withPost(
    Transformation.safe[Int] {
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
    }
  )

  val x2 = rewriter.map(x)

  '{
    println(${Expr(x.show)})
    println(${Expr(x2.show)})
    println()
    $x2
  }
}

object Transformation {
  /** A restrictive transformer that is guaranteed to generate type correct code */
  def safe[T: Type](transform: PartialFunction[Expr[T], Expr[T]]): Transformation =
    new SafeTransformation(transform)

    /** A general purpose transformer that may fail while transforming.
     *  It will check the type of the returned Expr and will throw if the type does not conform to the expected type.
     */
  def checked(transform: PartialFunction[Expr[Any], Expr[Any]]): Transformation =
    new CheckedTransformation(transform)
}

class CheckedTransformation(transform: PartialFunction[Expr[Any], Expr[Any]]) extends Transformation {
  def apply[T: Type](e: Expr[T])(given QuoteContext): Expr[T] = {
    transform.applyOrElse(e, identity) match {
      case '{ $e2: T } => e2
      case '{ $e2: $t } =>
        throw new Exception(
          s"""Transformed
            |${e.show}
            |into
            |${e2.show}
            |
            |Expected type to be
            |${summon[Type[T]].show}
            |but was
            |${t.show}
          """.stripMargin)
    }
  }
}

class SafeTransformation[U: Type](transform: PartialFunction[Expr[U], Expr[U]]) extends Transformation {
  def apply[T: Type](e: Expr[T])(given QuoteContext): Expr[T] = {
    e match {
      case '{ $e: U } => transform.applyOrElse(e, identity) match { case '{ $e2: T } => e2 }
      case e => e
    }
  }
}

abstract class Transformation {
  def apply[T: Type](e: Expr[T])(given QuoteContext): Expr[T]
}

private object Rewriter {
  def apply(): Rewriter = new Rewriter(Nil, Nil, false)
}

private class Rewriter private (preTransform: List[Transformation] = Nil, postTransform: List[Transformation] = Nil, fixPoint: Boolean) extends util.ExprMap {

  def withFixPoint: Rewriter =
    new Rewriter(preTransform, postTransform, fixPoint = true)
  def withPre(transform: Transformation): Rewriter =
    new Rewriter(transform :: preTransform, postTransform, fixPoint)
  def withPost(transform: Transformation): Rewriter =
    new Rewriter(preTransform, transform :: postTransform, fixPoint)

  def map[T](e: Expr[T])(given QuoteContext, Type[T]): Expr[T] = {
    val e2 = preTransform.foldLeft(e)((ei, transform) => transform(ei))
    val e3 = mapChildren(e2)
    val e4 = postTransform.foldLeft(e3)((ei, transform) => transform(ei))
    if fixPoint && !e4.matches(e) then map(e4) else e4
  }

}
