package dotty.tools.dotc.semanticdb.internal


object LiteParser {

  def readMessage[A <: SemanticdbGeneratedMessage](
      input: SemanticdbInputStream,
      message: A
  )(implicit
      cmp: SemanticdbGeneratedMessageCompanion[A]
  ): A = {
    val length = input.readRawVarint32()
    val oldLimit = input.pushLimit(length)
    val result: A = cmp.merge(message, input)
    input.checkLastTagWas(0)
    input.popLimit(oldLimit)
    result
  }

  def readMessage[A <: SemanticdbGeneratedMessage](
      input: SemanticdbInputStream
  )(implicit
      cmp: SemanticdbGeneratedMessageCompanion[A]
  ): A = {
    val length = input.readRawVarint32()
    val oldLimit = input.pushLimit(length)
    val result: A = cmp.parseFrom(input)
    input.checkLastTagWas(0)
    input.popLimit(oldLimit)
    result
  }

  @inline
  def preferredSemanticdbOutputStreamBufferSize(dataLength: Int) =
    dataLength min SemanticdbOutputStream.DEFAULT_BUFFER_SIZE
}
