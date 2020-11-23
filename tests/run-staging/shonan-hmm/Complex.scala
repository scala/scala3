
import scala.quoted._

case class Complex[T](re: T, im: T)

object Complex {
  implicit def complexIsLiftable[T: Type: Liftable]: Liftable[Complex[T]] = new Liftable {
    def toExpr(c: Complex[T]) = '{ Complex(${Expr(c.re)}, ${Expr(c.im)}) }
  }

 def of_complex_expr(x: Expr[Complex[Int]])(using Quotes): Complex[Expr[Int]] = Complex('{$x.re}, '{$x.im})
 def of_expr_complex(x: Complex[Expr[Int]])(using Quotes): Expr[Complex[Int]] = '{Complex(${x.re}, ${x.im})}


}