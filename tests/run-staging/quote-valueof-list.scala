import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {

    implicit def UnliftableInt: Unliftable[Int] = new {
      def fromExpr(n: Expr[Int]) = n match {
        case '{ 0 } => Some(0)
        case '{ 1 } => Some(1)
        case '{ 2 } => Some(1)
        case _ => None
      }
    }

    implicit def UnliftableBoolean: Unliftable[Boolean] = new Unliftable[Boolean] {
      def fromExpr(b: Expr[Boolean]) = b match {
        case '{ true } => Some(true)
        case '{ false } => Some(false)
        case _ => None
      }
    }

    implicit def UnliftableList[T: Unliftable: Type]: Unliftable[List[T]] = new {
      def fromExpr(xs: Expr[List[T]]) = (xs: Expr[Any]) match {
        case '{ ($xs1: List[T]).::($x) } =>
          for { head <- x.unlift; tail <- xs1.unlift }
          yield head :: tail
        case '{ Nil } => Some(Nil)
        case _ => None
      }
    }

    implicit def UnliftableOption[T: Unliftable: Type]: Unliftable[Option[T]] = new {
      def fromExpr(expr: Expr[Option[T]]) = expr match {
        case '{ Some[T]($x) } => for (v <- x.unlift) yield Some(v)
        case '{ None } => Some(None)
        case _ => None
      }
    }

    println(('{0}).unlift)
    println(('{1}).unlift)
    println(('{ println(); 1 }).unlift)

    println(('{true}).unlift)
    println(('{false}).unlift)
    println(('{ println(); false }).unlift)

    println(('{ Nil }: Expr[List[String]]).unlift)
    println(('{ "a" :: "b" :: "c" :: Nil }: Expr[List[String]]).unlift)

    println(('{ None }: Expr[Option[Int]]).unlift)
    println(('{ Some("abc") }: Expr[Option[String]]).unlift)

  }
}
