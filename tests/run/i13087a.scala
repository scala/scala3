trait SimpleTrait[T] {
  def myMethod(t: T): Int
  def doIt(t: T): Unit = {
    myMethod(t) // java.lang.AbstractMethodError: Method BadClass.myMethod(Ljava/lang/Object;)I is abstract
 }
}

abstract class SimpleClass[T] extends SimpleTrait[T] {
  def myMethod(t: String): Int = 5
}

class BadClass extends SimpleClass[String]

object Test {
  def main(args: Array[String]): Unit =
    (new BadClass).doIt("foobar")
}