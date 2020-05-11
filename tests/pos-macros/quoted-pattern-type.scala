import scala.quoted._

object Lib {

  def impl[T](using s: Scope)(arg: s.Expr[T])(using s.Type[T]): s.Expr[T] = {
    arg match {
      case e @ '{ $x: Boolean } =>
        e: s.Expr[T & Boolean]
        x: s.Expr[T & Boolean]
        e

      case e @ '{ println("hello"); $x } =>
        e: s.Expr[T]
        x: s.Expr[T]
        e

      case e @ '{ println("hello"); $x: T } =>
        e: s.Expr[T]
        x: s.Expr[T]
        e

      case e @ '{ Some($x: Int) } =>
        e: s.Expr[T & Some[Int]]
        x: s.Expr[Int]
        e

      case e @ '{ if ($x) ($y: Boolean) else ($z: Int) } =>
        e: s.Expr[T & (Boolean | Int)]
        y: s.Expr[T & Boolean]
        z: s.Expr[T & Int]
        e

      case e @ '{ if ($x) $y else $z } =>
        e: s.Expr[T]
        y: s.Expr[T]
        z: s.Expr[T]
        e

      case e @ '{ if ($x) $y else ($z: Int) } =>
        e: s.Expr[T & (T | Int)]
        y: s.Expr[T]
        z: s.Expr[T & Int]
        e

    }
  }
}
