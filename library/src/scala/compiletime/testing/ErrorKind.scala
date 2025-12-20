package scala.compiletime.testing

import language.experimental.captureChecking

/** An error can be either a parse-time or a typecheck-time. */
sealed trait ErrorKind // This should be an enum but currently, Dotty lib fails to
                       // compile with an obscure error.
object ErrorKind:
  case object Parser extends ErrorKind
  case object Typer extends ErrorKind
