package scala.compiletime

import scala.quoted._

package object testing {

  /** Whether the code type checks in the current context?
   *
   *  @param code The code to be type checked
   *
   *  @return false if the code has syntax error or type error in the current context, otherwise returns true.
   *
   *  The code should be a sequence of expressions or statements that may appear in a block.
   */
  inline def typeChecks(inline code: String): Boolean =
    error("`typeChecks` was not checked by the compiler")

}
