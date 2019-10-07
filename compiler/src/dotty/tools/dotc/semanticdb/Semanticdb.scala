package dotty.tools.dotc.semanticdb

import java.nio.file.Path

object Semanticdb {
  case class Range(
      startLine: Int,
      startCharacter: Int,
      endLine: Int,
      endCharacter: Int
  )
  trait ProtobufMessage {
    def writeTo(out: CodedOutputStream): Unit = {}
  }
  sealed abstract class Role
  object Role {
    case object Reference extends Role
    case object Definition extends Role
  }
  case class SymbolOccurrence(symbol: String, range: Range, role: Role) extends ProtobufMessage
  case class TextDocuments(documents: Seq[TextDocument])
  class CodedOutputStream() {
    def writeInt(): Unit = ()
    // ...
  }
  case class TextDocument(
      uri: String,
      md5: String,
      occurrences: Seq[SymbolOccurrence]
  ) {
     def writeTo(out: CodedOutputStream): Unit = {}
  }
}
