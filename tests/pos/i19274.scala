//> using options -Ycheck:mixin

object Diagnostic:
  trait OriginWarning(val origin: String):
    self: Warning =>
  object OriginWarning:
    val NoOrigin = "..."

  class LintWarning(msg: String, origin: String = OriginWarning.NoOrigin)
  extends Warning(msg), OriginWarning(origin)

  class Warning(msg: String) extends Diagnostic(msg)
class Diagnostic(val msg: String)
