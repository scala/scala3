class C {
  def t = println(ErrorHandler.defaultIfIOException("String")("String"))
}
object ErrorHandler {
  import java.io.IOException
  @inline
  def defaultIfIOException[T](default: => T)(closure: => T): T = try closure catch {
    case e: IOException => default
  }
}