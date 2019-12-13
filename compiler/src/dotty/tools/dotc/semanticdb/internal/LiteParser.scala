package dotty.tools.dotc.semanticdb.internal

import java.io.InputStream

object LiteParser {

  def readMessage[A](input: SemanticdbInputStream, message: SemanticdbMessage[A]): A = {
    val length    = input.readRawVarint32()
    val oldLimit  = input.pushLimit(length)
    val result: A = message.mergeFrom(input)
    input.checkLastTagWas(0)
    input.popLimit(oldLimit)
    result
  }

  @inline
  def preferredSemanticdbOutputStreamBufferSize(dataLength: Int) =
    dataLength min SemanticdbOutputStream.DEFAULT_BUFFER_SIZE
}
