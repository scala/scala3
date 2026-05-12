// Shadowing counterpart of `tests/pos/companion-inference-opaque.scala`.
// A local `Info` of unrelated type wins normal resolution and shadows the
// opaque alias's companion member.
import scala.language.experimental.companionScopeInference

object Lib:
  enum Severity:
    case INFO, WARN

  opaque type Level = Severity
  object Level:
    val Info: Level = Severity.INFO
    val Warn: Level = Severity.WARN

object OpaqueClient:
  import Lib.Level

  // Local with the same name as Level.Info.
  val Info = "informational"

  val l: Level = Info // error
