package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._

abstract class Language(val value: Int) extends SemanticdbEnum
object Language {
  case object UNKNOWN_LANGUAGE extends Language(0)
  case object SCALA extends Language(1)
  case object JAVA extends Language(2)
  case class Unrecognized(id: Int) extends Language(id)
  def fromValue(value: _root_.scala.Int): Language = value match {
    case 0 => UNKNOWN_LANGUAGE
    case 1 => SCALA
    case 2 => JAVA
    case __other => Unrecognized(__other)
  }
}
