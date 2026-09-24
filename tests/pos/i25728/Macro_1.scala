import scala.quoted.*

class Wrapper(val value: Int)

object MacroHelper:
  transparent inline def add[L, R, Out](
      inline lhs: L,
      inline rhs: R
  ): Out = ${ addMacro[L, R, Out]('lhs, 'rhs) }

  private def addMacro[L: Type, R: Type, Out: Type](
      lhs: Expr[L],
      rhs: Expr[R]
  )(using Quotes): Expr[Out] =
    '{
      val l = $lhs.asInstanceOf[Wrapper].value
      val r = $rhs.asInstanceOf[Wrapper].value
      new Wrapper(l + r).asInstanceOf[Out]
    }
