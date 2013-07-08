package dotty.tools
package dotc
package core

import util.SourcePosition
import reporting.Reporter

class Diagnostic(msgFn: => String, val pos: SourcePosition, severity: Reporter.Severity.Value) {
  lazy val msg: String = msgFn
}
