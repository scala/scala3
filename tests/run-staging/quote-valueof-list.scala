import scala.quoted.*
import scala.quoted.staging.*

object Test {

  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {

    implicit def FromExprInt: FromExpr[Int] = new {
      def unapply(n: Expr[Int])(using Quotes) = n match {
        case '{ 0 } => Some(0)
        case '{ 1 } => Some(1)
        case '{ 2 } => Some(1)
        case _ => None
      }
    }

    implicit def FromExprBoolean: FromExpr[Boolean] = new {
      def unapply(b: Expr[Boolean])(using Quotes) = b match {
        case '{ true } => Some(true)
        case '{ false } => Some(false)
        case _ => None
      }
    }

    implicit def FromExprList[T: FromExpr: Type]: FromExpr[List[T]] = new {
      def unapply(xs: Expr[List[T]])(using Quotes) = (xs: Expr[Any]) match {
        case '{ ($xs1: List[T]).::($x) } =>
          for { head <- x.value; tail <- xs1.value }
          yield head :: tail
        case '{ Nil } => Some(Nil)
        case _ => None
      }
    }

    implicit def FromExprOption[T: FromExpr: Type]: FromExpr[Option[T]] = new {
      def unapply(expr: Expr[Option[T]])(using Quotes) = expr match {
        case '{ Some[T]($x) } => for (v <- x.value) yield Some(v)
        case '{ None } => Some(None)
        case _ => None
      }
    }

    println(('{0}).value)
    println(('{1}).value)
    println(('{ println(); 1 }).value)

    println(('{true}).value)
    println(('{false}).value)
    println(('{ println(); false }).value)

    println(('{ Nil }: Expr[List[String]]).value)
    println(('{ "a" :: "b" :: "c" :: Nil }: Expr[List[String]]).value)

    println(('{ None }: Expr[Option[Int]]).value)
    println(('{ Some("abc") }: Expr[Option[String]]).value)

  }
}
