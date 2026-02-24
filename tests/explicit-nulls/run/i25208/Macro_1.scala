import scala.quoted.*

object NonNullFold:
  extension [A](inline arg: A | Null)
    inline def foldNN[B](inline default: => B)(inline fn: A => B): B =
      ${ foldNNImpl('arg, 'default, 'fn) }

  private def foldNNImpl[A: Type, B: Type](
      arg: Expr[A | Null],
      default: Expr[B],
      fn: Expr[A => B]
  )(using Quotes): Expr[B] =
    '{
      val t = $arg

      given CanEqual[A | Null, Null] = CanEqual.derived

      if t == null then $default
      else ${ Expr.betaReduce('{ $fn(t) }) }
    }
