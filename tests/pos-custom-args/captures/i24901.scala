import language.experimental.captureChecking
import caps.*

trait Rand extends SharedCapability

object Plan:
  def execute(test: Rand => Unit): Boolean =
    true

  val ok: (Rand => Unit) => Boolean =
    test =>
      execute(test) // error

  val err: (Rand => Unit) => Boolean =
    test =>
      execute(r => test(r)) // was error