package dotty.tools.dotc.semanticdb

abstract class Language(val value: Int) extends SemanticdbEnum
object Language {
  case object UNKNOWN_LANGUAGE extends Language(0)
  case object SCALA extends Language(1)
  case object JAVA extends Language(2)
}
