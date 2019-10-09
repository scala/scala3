package dotty.tools.dotc.semanticdb

abstract class Schema(val value: Int) extends SemanticdbEnum
object Schema {
  def fromValue(value: _root_.scala.Int): Schema = value match {
    case 0 => LEGACY
    case 3 => SEMANTICDB3
    case 4 => SEMANTICDB4
    case __other => Unrecognized(__other)
  }
  case object LEGACY extends Schema(0)
  case object SEMANTICDB3 extends Schema(3)
  case object SEMANTICDB4 extends Schema(4)
  case class Unrecognized(id: Int) extends Schema(id)
}
