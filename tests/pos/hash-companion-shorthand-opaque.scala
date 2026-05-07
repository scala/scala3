import scala.language.experimental.hashCompanionShorthand

// Module providing the opaque type. From outside this module the alias is
// abstract and `#X` should resolve against the alias's own companion (per SIP-80).
object Lib:
  enum Severity:
    case INFO, WARN, ERROR

  opaque type Level = Severity
  object Level:
    val Info: Level  = Severity.INFO
    val Warn: Level  = Severity.WARN
    val Error: Level = Severity.ERROR

object OpaqueClient:
  import Lib.Level

  // From outside Lib, Level is opaque — so `#Info` resolves on Level's companion.
  val l1: Level = #Info
  val l2: Level = #Warn

  def log(level: Level): Unit = ()
  log(#Error)
