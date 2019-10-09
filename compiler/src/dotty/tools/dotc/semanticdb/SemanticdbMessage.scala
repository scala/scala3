package dotty.tools.dotc.semanticdb

trait SemanticdbMessage[A] {
  def serializedSize: Int
  def writeTo(out: SemanticdbOutputStream): Unit
  def mergeFrom(in: SemanticdbInputStream): A
}
