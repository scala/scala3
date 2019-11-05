package scala.compiletime.testing

/** An error can be either a parse-time or a typecheck-time */
sealed trait ErrorKind  // TODO make this enum, so far not doable because ScalaJS compilation fails on it
object ErrorKind
  case object Parser extends ErrorKind
  case object Typer extends ErrorKind
