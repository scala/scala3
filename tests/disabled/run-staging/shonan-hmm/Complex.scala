
import scala.quoted._

case class Complex[T](re: T, im: T)

object Complex {
  implicit def complexIsLiftable[T](using s: Scope)(using s.Type[T], s.Liftable[T]): s.Liftable[Complex[T]] = new s.Liftable {
    def toExpr(c: Complex[T]) = '{ Complex(${Expr(c.re)}, ${Expr(c.im)}) }
  }

 def of_complex_expr(using s: Scope)(x: s.Expr[Complex[Int]]): Complex[s.Expr[Int]] = Complex('{$x.re}, '{$x.im})
 def of_expr_complex(using s: Scope)(x: Complex[s.Expr[Int]]): s.Expr[Complex[Int]] = '{Complex(${x.re}, ${x.im})}


}