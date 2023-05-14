
import scala.quoted.*

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T], qctx: Quotes): Expr[T] =
    '{ def y: T = $x; ${ loop('y) } }

  def loop2[T](x: Expr[T])(implicit t: Type[T], qctx: Quotes): Expr[T] =
    '{ def y(): T = $x; ${ loop2('{y()}) } }

  def loop3[T](x: Expr[T])(using Type[T], Quotes): Expr[T] =
    '{ def y(i: Int): T = $x; ${ loop2('{y(1)}) } }

  def loop4[T](x: Expr[T])(using Type[T], Quotes): Expr[T] =
  '{ def y(i: Int)(j: Int): T = $x; ${ loop2('{y(1)(2)}) } }
}
