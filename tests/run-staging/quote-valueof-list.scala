import scala.quoted._
import scala.quoted.staging._

object Test {

  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = usingNewScope {

    implicit def UnliftableInt(using s: Scope): s.Unliftable[Int] = new {
      def fromExpr(n: s.Expr[Int]): Option[Int] = n match {
        case '{ 0 } => Some(0)
        case '{ 1 } => Some(1)
        case '{ 2 } => Some(1)
        case _ => None
      }
    }

    implicit def UnliftableBoolean(using s: Scope): s.Unliftable[Boolean] = new s.Unliftable[Boolean] {
      def fromExpr(b: s.Expr[Boolean]): Option[Boolean] = b match {
        case '{ true } => Some(true)
        case '{ false } => Some(false)
        case _ => None
      }
    }

    implicit def UnliftableList[T](using s: Scope)(using s.Type[T], s.Unliftable[T]): s.Unliftable[List[T]] = new {
      def fromExpr(xs: s.Expr[List[T]]): Option[List[T]] = (xs: s.Expr[Any]) match {
        case '{ ($xs1: List[T]).::($x) } =>
          for { head <- x.unlift; tail <- xs1.unlift }
          yield head :: tail
        case '{ Nil } => Some(Nil)
        case _ => None
      }
    }

    implicit def UnliftableOption[T](using s: Scope)(using s.Type[T], s.Unliftable[T]): s.Unliftable[Option[T]] = new {
      def fromExpr(expr: s.Expr[Option[T]]): Option[Option[T]] = expr match {
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

    println(('{ Nil }: scope.Expr[List[String]]).unlift)
    println(('{ "a" :: "b" :: "c" :: Nil }: scope.Expr[List[String]]).unlift)

    println(('{ None }: scope.Expr[Option[Int]]).unlift)
    println(('{ Some("abc") }: scope.Expr[Option[String]]).unlift)

  }
}
