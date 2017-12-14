package scala.quoted

import scala.runtime.quoted.{Runner, Show}

abstract class Expr[T] extends Quoted {
  final def unary_~ : T = throw new Error("~ should have been compiled away")
  final def run(implicit runner: Runner[T]): T = runner.run(this)
  final def show(implicit runner: Show[T]): String = runner.run(this)
}

object Expr {
  implicit def toExpr[T](x: T)(implicit ev: Liftable[T]): Expr[T] =
    ev.toExpr(x)

  implicit class AsFunction[T, U](private val f: Expr[T => U]) extends AnyVal {
    def apply(x: Expr[T]): Expr[U] = ???
  }
}
