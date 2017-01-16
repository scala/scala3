import scala.annotation.internal

object Test {
  // @internal.link.CallGraphBounds(reachableClasses = 40, classesWithReachableMethods = 7, reachableMethods = 55)
  def main(args: Array[String]): Unit = {
    System.out.println(new Baz)
  }
}

class Baz extends Bar[Foo] {

  def get(): Foo = new Foo

  override def toString(): String = Foo.foo(this)
}