
import scala.quoted.*

case class Complex[T](re: T, im: T)

object Complex {
  implicit def complexIsToExpr[T: Type: ToExpr]: ToExpr[Complex[T]] = new ToExpr {
   def apply(c: Complex[T])(using Quotes) = '{ Complex(${Expr(c.re)}, ${Expr(c.im)}) }
  }

 def of_complex_expr(x: Expr[Complex[Int]])(using Quotes): Complex[Expr[Int]] = Complex('{$x.re}, '{$x.im})
 def of_expr_complex(x: Complex[Expr[Int]])(using Quotes): Expr[Complex[Int]] = '{Complex(${x.re}, ${x.im})}


}