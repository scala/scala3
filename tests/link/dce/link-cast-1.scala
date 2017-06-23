import scala.annotation.internal

object Test {
  // @internal.link.CallGraphBounds(reachableClasses = 31, classesWithReachableMethods = 7, reachableMethods = 55)
  def main(args: Array[String]): Unit = {
    val box = new Box[Foo](null)
    box.x = new Foo
    val box2 = box.asInstanceOf[Box[Bar]]
    box2.x = new Bar
    f(box.asInstanceOf[Box[Bla]])
    f(box2.asInstanceOf[Box[Bla]])
  }
  def f(b: Box[Bla]) = b.x.bla
}

class Box[T](var x: T)

class Foo extends Bla {
  override def bla: String = getClass.toString
}

class Bar extends Bla {
  override def bla: String = getClass.toString
}

trait Bla {
  def bla: String = ???
}
