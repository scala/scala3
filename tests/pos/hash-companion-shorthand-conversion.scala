import scala.language.experimental.hashCompanionShorthand
import scala.language.implicitConversions

object Conversion:

  enum LogLevel:
    case INFO, WARN, ERROR

  // Opaque-type bridge per SIP-80 §implicit-conversion bridging.
  opaque type ParserLogLevel = LogLevel
  object ParserLogLevel:
    export LogLevel.{INFO, WARN, ERROR}
    given Conversion[LogLevel, ParserLogLevel] = identity(_)

  def parseStrict(level: ParserLogLevel): Unit = ()

  // `#INFO` resolves against `ParserLogLevel`'s companion (export forwards),
  // produces a `LogLevel`, and the `Conversion` adapts it to `ParserLogLevel`.
  parseStrict(#INFO)
  parseStrict(#WARN)

  // Direct (no conversion needed).
  val l: LogLevel = #ERROR
