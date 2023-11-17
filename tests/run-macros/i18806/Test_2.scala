import scala.annotation.experimental

class Base:
  def foo(): Object = ???

@experimental
@gen1
class Sub extends Base
// > override def foo(): String = "hi"

@experimental
@main def Test(): Unit =
  val sub = new Sub
  println(sub.foo())
