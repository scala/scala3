import language.experimental.captureChecking
import caps.*

trait Rand extends SharedCapability

object Plan:
  def execute(test: Rand => Unit): Boolean =
    true

  val ok: (Rand => Unit) => Boolean =
    test =>
      execute(test) // was error

  val err: (Rand => Unit) => Boolean =
    test =>
      execute(r => test(r)) // was error

object PlanA:
  def execute(t: Rand ?=> Unit): Boolean =
    true

  val standard: (Rand ?=> Unit) => Boolean =
    test =>
      execute(cp ?=> test(using cp))

trait PlanB:
  def execute(test: Rand ?=> Unit): Boolean

object PlanB:
  def execute(t: Rand ?=> Unit): Boolean =
    true

  val standard: PlanB =
    test =>
      execute(cp ?=> test(using cp))

  val alt: PlanB =
    test => execute(test)
