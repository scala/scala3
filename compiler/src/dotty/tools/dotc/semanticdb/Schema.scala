package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.semanticdb.internal._

sealed trait Schema(val value: Int) extends SemanticdbEnum derives CanEqual

object Schema {

  case object LEGACY extends Schema(0)
  case object SEMANTICDB3 extends Schema(3)
  case object SEMANTICDB4 extends Schema(4)
  final case class Unrecognized(id: Int) extends Schema(id)

  def fromValue(value: Int): Schema = value match {
    case 0 => LEGACY
    case 3 => SEMANTICDB3
    case 4 => SEMANTICDB4
    case id => Unrecognized(id)
  }

}
