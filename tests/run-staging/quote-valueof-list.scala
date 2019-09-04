import scala.quoted._
import scala.quoted.staging._

object Test {

  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {

    implicit def ValueOfExprInt: ValueOfExpr[Int] = new {
      def apply(n: Expr[Int]) given QuoteContext: Option[Int] = n match {
        case '{ 0 } => Some(0)
        case '{ 1 } => Some(1)
        case '{ 2 } => Some(1)
        case _ => None
      }
    }

    implicit def ValueOfExprBoolean: ValueOfExpr[Boolean] = new ValueOfExpr[Boolean] {
      def apply(b: Expr[Boolean]) given QuoteContext: Option[Boolean] = b match {
        case '{ true } => Some(true)
        case '{ false } => Some(false)
        case _ => None
      }
    }

    implicit def ValueOfExprList[T: ValueOfExpr: Type]: ValueOfExpr[List[T]] = new {
      def apply(xs: Expr[List[T]]) given QuoteContext: Option[List[T]] = (xs: Expr[Any]) match {
        case '{ ($xs1: List[T]).::($x) } =>
          for { head <- x.getValue; tail <- xs1.getValue }
          yield head :: tail
        case '{ Nil } => Some(Nil)
        case _ => None
      }
    }

    implicit def ValueOfExprOption[T: ValueOfExpr: Type]: ValueOfExpr[Option[T]] = new {
      def apply(expr: Expr[Option[T]]) given QuoteContext: Option[Option[T]] = expr match {
        case '{ Some[T]($x) } => for (v <- x.getValue) yield Some(v)
        case '{ None } => Some(None)
        case _ => None
      }
    }

    println(('{0}).getValue)
    println(('{1}).getValue)
    println(('{ println(); 1 }).getValue)

    println(('{true}).getValue)
    println(('{false}).getValue)
    println(('{ println(); false }).getValue)

    println(('{ Nil }: Expr[List[String]]).getValue)
    println(('{ "a" :: "b" :: "c" :: Nil }: Expr[List[String]]).getValue)

    println(('{ None }: Expr[Option[Int]]).getValue)
    println(('{ Some("abc") }: Expr[Option[String]]).getValue)

  }
}
