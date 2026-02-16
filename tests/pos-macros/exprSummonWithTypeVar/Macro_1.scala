import scala.compiletime.{erasedValue, summonFrom}

import scala.quoted._

inline given summonAfterTypeMatch: [T] => Any =
  ${ summonAfterTypeMatchExpr[T] }

private def summonAfterTypeMatchExpr[T: Type](using Quotes): Expr[Any] =
  Expr.summon[Foo[T]].get

trait Foo[T]

given IntFoo: [T <: Int] => Foo[T] = ???
