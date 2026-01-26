import scala.concurrent.duration.*

object i24750:

  def f(duration: FiniteDuration): Unit =
    duration.unit match // warn
      case NANOSECONDS =>
      case MICROSECONDS =>
      case MILLISECONDS =>
      case SECONDS =>
      case MINUTES =>
      case HOURS =>
