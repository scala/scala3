package dotty.tools
package dotc
package reporting
package diagnostic

import util.{SourcePosition, NoSourcePosition}
import core.Contexts.Context

object Message {
  implicit def toNoExplanation(str: String): Message =
    new NoExplanation(str)
}

abstract class Message(val errorId: String) { self =>
  import messages._

  def msg: String
  def kind: String
  def explanation: String

  def container(c: String) =
    if (kind == "") c
    else s"$kind $c"

  def mapMsg(f: String => String) = new Message(errorId) {
    val msg = f(self.msg)
    val kind = self.kind
    val explanation = self.explanation
  }

  def error(pos: SourcePosition) =
    new Error(self, pos, container("Error"), explanation)

  def warning(pos: SourcePosition) =
    new Warning(self, pos, container("Warning"), explanation)

  def info(pos: SourcePosition) =
    new Info(self, pos, container("Info"), explanation)

  def featureWarning(pos: SourcePosition) =
    new FeatureWarning(self, pos, container("Feature Warning"), explanation)

  def uncheckedWarning(pos: SourcePosition) =
    new UncheckedWarning(self, pos, container("Unchecked Warning"), explanation)

  def deprecationWarning(pos: SourcePosition) =
    new DeprecationWarning(self, pos, container("Deprecation Warning"), explanation)

  def migrationWarning(pos: SourcePosition) =
    new MigrationWarning(self, pos, container("Migration Warning"), explanation)
}

class NoExplanation(val msg: String) extends Message("") {
  val explanation = ""
  val kind = ""
}

object NoExplanation {
  def unapply(m: Message): Option[Message] =
    if (m.explanation == "") Some(m)
    else None
}
