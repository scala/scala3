package test

import scala.quoted.*

object Macro:
  def covImpl(arg: Expr[Any])(using Quotes): Expr[Any] = arg match // Covariant (List)
    case '{ $h : List[h] } => '{ $h : List[h] }

  def invImpl(arg: Expr[Any])(using Quotes): Expr[Any] = arg match // Invariant (Set)
    case '{ $h : Set[h] } => '{ $h : Set[h] }

  transparent inline def cov(inline arg: Any): Any = ${ covImpl('arg) }
  transparent inline def inv(inline arg: Any): Any = ${ invImpl('arg) }
