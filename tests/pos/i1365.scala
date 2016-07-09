import scala.collection.mutable.ArrayBuffer

trait Message[M]
class Script[S] extends ArrayBuffer[Message[S]] with Message[S]

class Test[A] {
  def f(cmd: Message[A]): Unit = cmd match {
    case s: Script[_] => s.iterator.foreach(x => f(x))
  }
  def g(cmd: Message[A]): Unit = cmd match {
    case s: Script[z] => s.iterator.foreach(x => g(x))
  }
}
