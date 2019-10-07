package dotty.tools.dotc.semanticdb

trait SemanticdbMessage {
  def serializedSize: Int
  def writeTo(out: SemanticdbOutputStream): Unit
}
