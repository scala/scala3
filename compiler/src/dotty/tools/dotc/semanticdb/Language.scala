package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._

sealed trait Language(val value: Int) extends SemanticdbEnum derives CanEqual

object Language {

  case object UNKNOWN_LANGUAGE extends Language(0)
  case object SCALA extends Language(1)
  case object JAVA extends Language(2)
  final case class Unrecognized(id: Int) extends Language(id)

  def fromValue(value: Int): Language = value match {
    case 0 => UNKNOWN_LANGUAGE
    case 1 => SCALA
    case 2 => JAVA
    case id => Unrecognized(id)
  }

}
