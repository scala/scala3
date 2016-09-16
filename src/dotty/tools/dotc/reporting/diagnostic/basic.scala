package dotty.tools
package dotc
package reporting
package diagnostic

import dotc.core.Contexts.Context
import util.{SourceFile, NoSource}
import util.{SourcePosition, NoSourcePosition}
import config.Settings.Setting
import interfaces.Diagnostic.{ERROR, WARNING, INFO}

object basic {

  class Error(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Error",
    explanation: String = ""
  ) extends Message(msgFn, pos, ERROR, kind, explanation)

  class Warning(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Warning",
    explanation: String = ""
  ) extends Message(msgFn, pos, WARNING, kind, explanation)

  class Info(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Info",
    explanation: String = ""
  ) extends Message(msgFn, pos, INFO, kind, explanation)

  abstract class ConditionalWarning(
    msgFn: => String,
    pos: SourcePosition,
    kind: String,
    explanation: String = ""
  ) extends Warning(msgFn, pos, kind, explanation) {
    def enablingOption(implicit ctx: Context): Setting[Boolean]
  }

  class FeatureWarning(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Feature Warning",
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, kind, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.feature
  }

  class UncheckedWarning(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Unchecked Warning",
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, kind, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.unchecked
  }

  class DeprecationWarning(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Deprecation Warning",
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, kind, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.deprecation
  }

  class MigrationWarning(
    msgFn: => String,
    pos: SourcePosition,
    kind: String = "Migration Warning",
    explanation: String = ""
  ) extends ConditionalWarning(msgFn, pos, kind, explanation) {
    def enablingOption(implicit ctx: Context) = ctx.settings.migration
  }

}
