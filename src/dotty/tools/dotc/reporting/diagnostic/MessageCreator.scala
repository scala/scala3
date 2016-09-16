package dotty.tools
package dotc
package reporting
package diagnostic

import util.{SourcePosition, NoSourcePosition}
import core.Contexts.Context

object MessageCreator {
  implicit class DiagnosticContext(val c: Context) extends AnyVal {
    def shouldExplain(msg: MessageCreator): Boolean = {
      implicit val ctx: Context = c
      msg match {
        case NoExplanation(_) => false
        case _ => ctx.settings.explain.value
      }
    }
  }

  implicit def toNoExplanation(str: String) =
    new NoExplanation(str)
}

trait MessageCreator {
  import messages._

  def msg: String
  def kind: String
  def explanation: String

  def error(pos: SourcePosition) =
    new Error(msg, pos, kind, explanation)

  def warning(pos: SourcePosition) =
    new Warning(msg, pos, kind, explanation)

  def info(pos: SourcePosition) =
    new Info(msg, pos, kind, explanation)

  def featureWarnign(pos: SourcePosition) =
    new FeatureWarning(msg, pos, kind, explanation)

  def uncheckedWarning(pos: SourcePosition) =
    new UncheckedWarning(msg, pos, kind, explanation)

  def deprecationWarning(pos: SourcePosition) =
    new DeprecationWarning(msg, pos, kind, explanation)

  def migrationWarning(pos: SourcePosition) =
    new MigrationWarning(msg, pos, kind, explanation)
}

class NoExplanation(val msg: String) extends MessageCreator {
  val explanation = ""
  val kind = ""
}

object NoExplanation {
  def unapply(m: MessageCreator): Option[MessageCreator] =
    if (m.explanation == "") Some(m)
    else None
}
