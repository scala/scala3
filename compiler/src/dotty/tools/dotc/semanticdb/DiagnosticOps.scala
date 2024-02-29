package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.{semanticdb => s}
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.dotc.core.Contexts.Context
import scala.annotation.internal.sharable

object DiagnosticOps:
  @sharable private val asciiColorCodes = "\u001B\\[[;\\d]*m".r
  extension (d: Diagnostic)
    def toSemanticDiagnostic: s.Diagnostic =
      val severity = d.level match
        case ERROR => s.Diagnostic.Severity.ERROR
        case WARNING => s.Diagnostic.Severity.WARNING
        case INFO => s.Diagnostic.Severity.INFORMATION
        case _ => s.Diagnostic.Severity.INFORMATION
      val msg = asciiColorCodes.replaceAllIn(d.msg.message, m => "")
      s.Diagnostic(
        range = Scala3.range(d.pos.span, d.pos.source),
        severity = severity,
        message = msg
      )
