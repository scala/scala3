import scala.quoted._

object Lib {

  def impl[T: Type](arg: Expr[T])(using Quotes): Expr[T] = {
    arg match {
      case '{ $x: Boolean } as e =>
        e: Expr[T & Boolean]
        x: Expr[T & Boolean]
        e

      case '{ println("hello"); $x } as e =>
        e: Expr[T]
        x: Expr[T]
        e

      case '{ println("hello"); $x: T } as e =>
        e: Expr[T]
        x: Expr[T]
        e

      case '{ Some($x: Int) } as e =>
        e: Expr[T & Some[Int]]
        x: Expr[Int]
        e

      case '{ if ($x) ($y: Boolean) else ($z: Int) } as e =>
        e: Expr[T & (Boolean | Int)]
        y: Expr[T & Boolean]
        z: Expr[T & Int]
        e

      case '{ if ($x) $y else $z } as e =>
        e: Expr[T]
        y: Expr[T]
        z: Expr[T]
        e

      case '{ if ($x) $y else ($z: Int) } as e =>
        e: Expr[T & (T | Int)]
        y: Expr[T]
        z: Expr[T & Int]
        e

    }
  }
}
