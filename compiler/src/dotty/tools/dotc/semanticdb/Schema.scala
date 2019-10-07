package dotty.tools.dotc.semanticdb

abstract class Schema(val value: Int) extends SemanticdbEnum
object Schema {
  case object LEGACY extends Schema(0)
  case object SEMANTICDB3 extends Schema(3)
  case object SEMANTICDB4 extends Schema(4)
}
