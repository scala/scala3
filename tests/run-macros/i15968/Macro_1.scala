import scala.quoted.*

inline def macroPolyFun[A](inline arg: A, inline f: [Z] => Z => String): String =
  ${ macroPolyFunImpl[A]('arg, 'f) }

private def macroPolyFunImpl[A: Type](arg: Expr[A], f: Expr[[Z] => Z => String])(using Quotes): Expr[String] =
  Expr(Expr.betaReduce('{ $f($arg) }).show)


inline def macroFun[A](inline arg: A, inline f: A => String): String =
  ${ macroFunImpl[A]('arg, 'f) }

private def macroFunImpl[A: Type](arg: Expr[A], f: Expr[A => String])(using Quotes): Expr[String] =
  Expr(Expr.betaReduce('{ $f($arg) }).show)

