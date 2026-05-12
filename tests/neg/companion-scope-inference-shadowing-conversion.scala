// Shadowing counterpart of `tests/pos/companion-inference-conversion.scala`.
// A local `INFO` of unrelated type wins normal resolution, so companion
// inference does not fire and the conversion bridge is never reached.
import scala.language.experimental.companionScopeInference
import scala.language.implicitConversions

object Conversion:

  enum LogLevel:
    case INFO, WARN

  opaque type ParserLogLevel = LogLevel
  object ParserLogLevel:
    export LogLevel.{INFO, WARN}
    given Conversion[LogLevel, ParserLogLevel] = identity(_)

  def parseStrict(level: ParserLogLevel): Unit = ()

  // Local with the same name as a companion-exported case.
  val INFO = 0

  parseStrict(INFO) // error
