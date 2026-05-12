import scala.language.experimental.companionScopeInference
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

  // `INFO` is not in scope; companion scope inference resolves it against
  // ParserLogLevel's companion (export forwards), producing a LogLevel,
  // and the Conversion adapts it to ParserLogLevel.
  parseStrict(INFO)
  parseStrict(WARN)

  // Direct (no conversion needed).
  val l: LogLevel = ERROR
