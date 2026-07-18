final class Flow[In, Out]:
  def collect[T](pf: PartialFunction[Out, T]): Flow[In, T] = ???

object Flow:
  def create[T](): Flow[T, T] = ???


abstract class Message:
  def asTextMessage: TextMessage

abstract class TextMessage extends Message

abstract class JavaPartialFunction[A, B] extends PartialFunction[A, B]:
  @throws(classOf[Exception]) // required, compiles without annotation
  def apply(x: A, isCheck: Boolean): B

  final def isDefinedAt(x: A): Boolean = ???
  final override def apply(x: A): B = ???
  final override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = ???

