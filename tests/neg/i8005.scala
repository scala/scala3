package nio
abstract class Buffer {
  val isReadOnly: Boolean
  def foo(): Unit
  def bar(): Unit
}

class ByteBuffer extends Buffer {  // error: ByteBufer needs to be abstract since `bar` is not defined
  private[nio] val isReadOnly: Boolean = false // error
  protected def foo(): Unit = ()  // error
  private def bar(): Unit = () // error
}
