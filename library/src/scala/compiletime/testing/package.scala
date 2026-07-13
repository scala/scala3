package scala.compiletime
package testing

import language.experimental.captureChecking

/** Whether the code type checks in the current context?
 *
 *  An inline definition with a call to `typeChecks` should be transparent.
 *
 *  @param code a string literal containing the Scala code to type-check at compile time
 *  @return `true` if the code type-checks successfully, `false` if the code has a syntax or type error in the current context.
 *
 *  The code should be a sequence of expressions or statements that may appear in a block.
 */
transparent inline def typeChecks(inline code: String): Boolean =
  // implemented in package dotty.tools.dotc.typer.Inliner.Intrinsics
  error("Compiler bug: `typeChecks` was not checked by the compiler")

/** Whether the code type checks in the current context, running the
 *  compilation up to and including the given compiler phase?
 *
 *  This overload behaves like `typeChecks(code)`, but in addition it runs the
 *  enabled compiler-plugin phases (`scala.tools.dotc.plugins.PluginPhase`,
 *  registered via `-Xplugin`) that are scheduled at or before `stopAfterPhase`.
 *  This makes it possible to test diagnostics emitted by compiler plugins, which
 *  the single-argument `typeChecks` does not run.
 *
 *  An inline definition with a call to `typeChecks` should be transparent.
 *
 *  @param code a string literal containing the Scala code to type-check at compile time
 *  @param stopAfterPhase the name of the compiler phase up to (and including) which the
 *                        code should be checked; plugin phases scheduled at or before it are run
 *  @return `true` if the code type-checks successfully, `false` if the code has a syntax or
 *          type error, or an error reported by a plugin phase, in the current context.
 *
 *  The code should be a sequence of expressions or statements that may appear in a block.
 */
transparent inline def typeChecks(inline code: String, inline stopAfterPhase: String): Boolean =
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
 *  @param code a string literal containing the Scala code to type-check at compile time
 *  @return an empty list if the code type-checks successfully, or a list of `Error` values describing the errors encountered during parsing and type-checking.
 *
 *  The code should be a sequence of expressions or statements that may appear in a block.
 */
transparent inline def typeCheckErrors(inline code: String): List[Error] =
  // implemented in package dotty.tools.dotc.typer.Inliner.Intrinsics
  error("Compiler bug: `typeCheckErrors` was not checked by the compiler")

/** Whether the code type checks in the current context, running the compilation
 *  up to and including the given compiler phase? If not, returns a list of errors
 *  encountered on compilation.
 *  IMPORTANT: No stability guarantees are provided on the format of these
 *  errors. This means the format and the API may change from
 *  version to version. This API is to be used for testing purposes
 *  only.
 *
 *  This overload behaves like `typeCheckErrors(code)`, but in addition it runs the
 *  enabled compiler-plugin phases (`scala.tools.dotc.plugins.PluginPhase`,
 *  registered via `-Xplugin`) that are scheduled at or before `stopAfterPhase`.
 *  This makes it possible to test diagnostics emitted by compiler plugins, which
 *  the single-argument `typeCheckErrors` does not run.
 *
 *  An inline definition with a call to `typeCheckErrors` should be transparent.
 *
 *  @param code a string literal containing the Scala code to type-check at compile time
 *  @param stopAfterPhase the name of the compiler phase up to (and including) which the
 *                        code should be checked; plugin phases scheduled at or before it are run
 *  @return an empty list if the code type-checks successfully, or a list of `Error` values
 *          describing the errors encountered during parsing, type-checking and the run
 *          plugin phases.
 *
 *  The code should be a sequence of expressions or statements that may appear in a block.
 */
transparent inline def typeCheckErrors(inline code: String, inline stopAfterPhase: String): List[Error] =
  // implemented in package dotty.tools.dotc.typer.Inliner.Intrinsics
  error("Compiler bug: `typeCheckErrors` was not checked by the compiler")
