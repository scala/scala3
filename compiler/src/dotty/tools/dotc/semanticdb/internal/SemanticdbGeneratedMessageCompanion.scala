package dotty.tools.dotc.semanticdb.internal


// https://github.com/scalapb/ScalaPB/blob/194463272125b872b99d4902b7712355a53e96c4/scalapb-runtime/src/main/scala/scalapb/GeneratedMessageCompanion.scala#L61-L68
trait SemanticdbGeneratedOneof extends Any with Product with Serializable {
  type ValueType
  def number: Int
  def isDefined: Boolean
  def isEmpty: Boolean
  def value: ValueType
  def valueOption: Option[ValueType] = if (isDefined) Some(value) else None
}

// https://github.com/scalapb/ScalaPB/blob/194463272125b872b99d4902b7712355a53e96c4/scalapb-runtime/src/main/scala/scalapb/GeneratedMessageCompanion.scala#L72-L138
trait SemanticdbGeneratedMessage extends Any with Product with Serializable {
  def serializedSize: Int

  def writeTo(output: SemanticdbOutputStream): Unit

  /** Serializes the messgae and returns a byte array containing its raw bytes */
  final def toByteArray: Array[Byte] = {
    val a = new Array[Byte](serializedSize)
    val outputStream = SemanticdbOutputStream.newInstance(a)
    writeTo(outputStream)
    outputStream.checkNoSpaceLeft()
    a
  }
}

trait SemanticdbGeneratedSealedOneof
    extends Any
    with Product
    with Serializable {
  type MessageType <: SemanticdbGeneratedMessage
  def isEmpty: Boolean
  def isDefined: Boolean
  def asMessage: MessageType
}

trait SemanticdbGeneratedEnum extends Any with Product with Serializable {
  type EnumType <: SemanticdbGeneratedEnum

  def value: Int

  def index: Int

  def name: String

  override def toString = name

  def isUnrecognized: Boolean = false

}

trait SemanticdbUnrecognizedEnum extends SemanticdbGeneratedEnum {
  def name = "UNRECOGNIZED"

  def index = -1

  override def isUnrecognized: Boolean = true
}

trait SemanticdbGeneratedMessageCompanion[A <: SemanticdbGeneratedMessage]
    extends Serializable {
  self =>
  type ValueType = A

  /** Parses a message from a CodedInputStream. */
  def parseFrom(input: SemanticdbInputStream): A

  def parseFrom(input: Array[Byte]): A = parseFrom(
    SemanticdbInputStream.newInstance(input)
  )

  /** Merges the given message with the additional fields in the steam. */
  def merge(a: A, input: SemanticdbInputStream): A = {
    parseFrom(a.toByteArray ++ parseFrom(input).toByteArray)
  }
}
