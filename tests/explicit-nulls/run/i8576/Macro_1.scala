import scala.quoted.*
type Opt[A] = A | Null
object Opt {
  extension [A, B](inline lhs: Opt[A]) inline def map(inline rhs: A => B): Opt[B] = ${ mapMacro('lhs, 'rhs) }
  private def mapMacro[A: Type, B: Type](lhs: Expr[Opt[A]], rhs: Expr[A => B])(using Quotes): Expr[Opt[B]] =
    lhs match
      case '{ $v: Opt[A] } =>
        '{
          val result = $v
          if result != null then
            ${Expr.betaReduce('{$rhs(result)})}
          else
            null
        }
}
