package scala.compiletime
package testing

import language.experimental.captureChecking

/** Whether the code type checks in the current context?
 *
 *  An inline definition with a call to `typeChecks` should be transparent.
 *
 *  @param code The code to be type checked
 *
 *  @return `false` if the code has syntax error or type error in the current context, `true` otherwise.
 *
 *  The code should be a sequence of expressions or statements that may appear in a block.
 */
transparent inline def typeChecks(inline code: String): Boolean =
  // implemented in package dotty.tools.dotc.typer.Inliner.Intrinsics
  error("Compiler bug: `typeChecks` was not checked by the compiler")

/** Whether the code type checks in the current context? If not,
 *  returns a list of errors encountered on compilation.
 *  IMPORTANT: No stability guarantees are provided on the format of these
 *  errors. This means the format and the API may change from
 *  version to version. This API is to be used for testing purposes
 *  only.
 *
 *  An inline definition with a call to `typeCheckErrors` should be transparent.
 *
 *  @param code The code to be type checked
 *
 *  @return a list of errors encountered during parsing and typechecking.
 *
 *  The code should be a sequence of expressions or statements that may appear in a block.
 */
transparent inline def typeCheckErrors(inline code: String): List[Error] =
  // implemented in package dotty.tools.dotc.typer.Inliner.Intrinsics
  error("Compiler bug: `typeCheckErrors` was not checked by the compiler")
