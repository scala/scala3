import caps.{any, Classifier, Control}

// Section 6.2 of "Classifying Capabilities": real-world effect-exclusion patterns
// catalogued by Lutze et al., expressed with classifier exclusion.

// amule: a data-reading callback that must not invoke any CSafeIOException.
trait CSafeIOException extends Classifier, Control
case class CFileDataIO[T](doRead: (T, Long) -> {any.except[CSafeIOException]} Long)

// dune: runWithErrHandler requires the body not to create a nested error handler,
// and the no-raise handler not to use any control (exception) capability.
trait ErrHandler extends Classifier, Control
object RunWithErrHandler extends ErrHandler:
  type Fiber[_]
  type Raise
  def apply[A](
    f: this.Raise ?->{any.except[ErrHandler]} A,
    handleErrNoRaise: Exception ->{any.except[Control]} Fiber[Unit],
  ): Fiber[A] = ???
