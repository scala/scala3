import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions
import scala.reflect.ClassTag

class AnyShouldWrapper[T]

class Test {
  implicit def convertToAnyShouldWrapper[T](o: T)(implicit pos: Position): AnyShouldWrapper[T] = ???

  def expectMsgType[T](implicit t: ClassTag[T]): T = ???
  def expectMsgType[T](max: FiniteDuration)(implicit t: ClassTag[T]): T = ???

  def test(): Unit = {
    expectMsgType[Int]: AnyShouldWrapper[Int]
  }
}
