package scala.compiletime.testing

import language.experimental.captureChecking

/** Represents a compile-time error.
 *
 *  @see scala.compiletime.testing.typeCheckErrors
 *
 *  IMPORTANT: No stability guarantees are provided on the format of these
 *  errors. This means the format and the API may change from
 *  version to version. This API is to be used for testing purposes
 *  only.
 *
 *  @param message the error message produced by the compiler
 *  @param lineContent the source line containing the error
 *  @param column the zero-based column position within `lineContent` where the error occurred
 *  @param kind the phase in which the error occurred, either `ErrorKind.Parser` or `ErrorKind.Typer`
 */
final case class Error(message: String, lineContent: String, column: Int, kind: ErrorKind)
