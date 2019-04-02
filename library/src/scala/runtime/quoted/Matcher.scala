package scala.runtime.quoted

import scala.quoted.Expr
import scala.tasty.Reflection

/** THIS IS A PLACEHOLDER
 */
object Matcher {
  def unapply[Tup <: Tuple](scrut: Expr[_])(implicit pattern: Expr[_], reflection: Reflection): Option[Tup] = ???
}
