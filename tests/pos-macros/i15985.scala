package anorm.macros
sealed trait Row
sealed trait SqlResult[A]

import scala.quoted.{ Expr, Quotes, Type }

private[anorm] object RowParserImpl {
  def apply[A](using q:Quotes)(using a: Type[A]): Expr[Row => SqlResult[A]] = {
    import q.reflect.*

    inline def f1: Expr[SqlResult[A]] =
      Match(???, ???).asExprOf[SqlResult[A]] // (using Type.of[anorm.macros.SqlResult[A]] })

    inline def f2: Expr[SqlResult[A]] =
      Match(???, ???).asExprOf[SqlResult[A]](using Type.of[SqlResult[A]])
      // In Staging phase it becomes
      //  ..asExprOf[..](using Type.of[{ @SplicedType type a$_$3 = a.Underlying; anorm.macros.SqlResult[a$_$3] }])

    inline def f3(using Type[SqlResult[A]]): Expr[SqlResult[A]] =
      Match(???, ???).asExprOf[SqlResult[A]]

    f1
    f2
    f3

    ???
  }
}
