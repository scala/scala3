package dotty.tools.dotc.reporting

class Severity(val level: Int) extends AnyVal {
  import Severity._
  
  override def toString = this match {
    case VerboseINFO => "VerboseINFO"
    case INFO => "INFO"
    case DeprecationWARNING => "DeprecationWARNING"
    case UncheckedWARNING => "UncheckedWARNING"
    case FeatureWARNING => "FeatureWARNING"
    case WARNING => "WARNING"
    case ERROR => "ERROR"
  }
}

object Severity {
  final val VerboseINFO = new Severity(0)
  final val INFO = new Severity(1)
  final val DeprecationWARNING = new Severity(2)
  final val UncheckedWARNING = new Severity(3)
  final val FeatureWARNING = new Severity(4)
  final val WARNING = new Severity(5)
  final val ERROR = new Severity(6)
}
