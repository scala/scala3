abstract class Foo[T](defaultValue: => T, arg1: Int = 1, arg2: Int = 2):
  def getDefault: T = defaultValue
  def getArg1: Int = arg1
  def getArg2: Int = arg2

class MyValue:
  override def toString = "MyValue"

object TheObject extends Foo[MyValue](TheObject.value, arg2 = 3):
  val value = new MyValue

@main def Test =
  println(TheObject.getDefault)
  println(TheObject.getArg1)
  println(TheObject.getArg2)
