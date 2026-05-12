// SIP-80 companion-inference draft, §Silent shadowing. This test captures
// the canonical case discussed in the proposal: `Error` is in scope
// everywhere as `java.lang.Error` (Predef-imported transitively), so a
// companion field named `Error` is shadowed; normal name resolution finds
// the JDK class first, and companion scope inference (which is a fallback) never
// fires.
//
// The user receives the standard "Java defined class Error is not a value"
// diagnostic. The SIP's normative diagnostic-quality requirement is to
// additionally surface a hint such as:
//
//     note: `Lib.Level`'s companion has a member named `Error`;
//           qualify as `Lib.Level.Error` if you meant that.
//
// That diagnostic enrichment is future work. The test captures the current
// behaviour so any future improvement shows up as a check-file diff.
import scala.language.experimental.companionScopeInference

object Lib:
  enum Severity:
    case INFO, WARN, ERROR

  opaque type Level = Severity
  object Level:
    val Info:  Level = Severity.INFO
    val Warn:  Level = Severity.WARN
    val Error: Level = Severity.ERROR

object Client:
  import Lib.Level

  def log(level: Level): Unit = ()

  // Normal name resolution picks `java.lang.Error` (in scope via Predef
  // inheritance). Companion inference does NOT fire here — it only runs
  // when normal lookup returns nothing. The diagnostic reads:
  //   "Java defined class Error is not a value"
  log(Error) // error
