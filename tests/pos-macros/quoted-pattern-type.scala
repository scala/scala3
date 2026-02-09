import scala.quoted.*

object Lib {

  def impl[T: Type](arg: Expr[T])(using Quotes): Expr[T] = {
    arg match {
      case e @ '{ $x: Boolean } =>
        e: Expr[T & Boolean]
        x: Expr[T & Boolean]
        e

      case e @ '{ println("hello"); $x } =>
        e: Expr[T]
        x: Expr[T]
        e

      case e @ '{ println("hello"); $x: T } =>
        e: Expr[T]
        x: Expr[T]
        e

      case e @ '{ Some($x: Int) } =>
        e: Expr[T] & Expr[Some[Int]]
        x: Expr[Int]
        e

      case e @ '{ if ($x) ($y: Boolean) else ($z: Int) } =>
        e: Expr[T & (Boolean | Int)]
        y: Expr[T & Boolean]
        z: Expr[T & Int]
        e

      case e @ '{ if ($x) $y else $z } =>
        e: Expr[T]
        y: Expr[T]
        z: Expr[T]
        e

      case e @ '{ if ($x) $y else ($z: Int) } =>
        e: Expr[T & (T | Int)]
        y: Expr[T]
        z: Expr[T & Int]
        e

    }
  }
}
