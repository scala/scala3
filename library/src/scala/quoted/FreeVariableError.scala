package scala.quoted

/** Exception is thrown when trying to evaluate the contents of an expression
 *  that has free variables (i.e. has some local references to code that is
 *  outside of the expression). This can happen on Expr that passed as arguments to
 *  an inline macro.
 */
class FreeVariableError(val name: String) extends QuoteError("Free variable " + name + " could not be handled")
