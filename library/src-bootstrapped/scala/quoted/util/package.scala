package scala.quoted

import scala.internal.quoted.showName

package object util {

  /** Genrate the code `'{ val `<name>` = $expr; ${ body('<name>) } }` with the given name */
  def let[T: Type, U: Type](name: String)(expr: Expr[T])(body: Expr[T] => Expr[U])(given QuoteContext): Expr[U] = '{
     @showName(${Expr(name)}) val x = $expr
     ${ body('x) }
  }

  /** Genrate the code `'{ val x$<i> = $expr; ${ body('x$<i>) } }` for a fresh i */
  def let[T: Type, U: Type](expr: Expr[T])(body: Expr[T] => Expr[U])(given qctx: QuoteContext): Expr[U] =
    let("x$" + qctx.nextIndex())(expr)(body)

}
