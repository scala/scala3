import scala.language.experimental.companionScopeInference

// Module providing the opaque type. From outside this module the alias is
// abstract and a bare identifier resolves against the alias's own companion.
object Lib:
  enum Severity:
    case INFO, WARN, ERROR

  opaque type Level = Severity
  object Level:
    val Info: Level = Severity.INFO
    val Warn: Level = Severity.WARN
    val Bad:  Level = Severity.ERROR
    // (Not naming this `Error` — Predef imports `java.lang.Error` into scope,
    // which would shadow companion scope inference. The SIP acknowledges this
    // shadowing case under §Silent shadowing.)

object OpaqueClient:
  import Lib.Level

  // From outside Lib, Level is opaque — so `Info` resolves on Level's
  // companion (not on Severity's companion).
  val l1: Level = Info
  val l2: Level = Warn

  def log(level: Level): Unit = ()
  log(Bad)
