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

// retain old default for bincompat, but allow new default
object izumi:
  private def copy(ref: String): String = "hello, world"
  @deprecated("bincompat only", "20.02.2023")
  private def copy$default$1: String = "hello, world"
  def copy(ref: Int = 42): String = s"hello, $ref"
