abstract class Foo(defaultValue: => Int, arg1: Int = 1, arg2: Int = 2):
  def getDefault: Int = defaultValue
  def getArg1: Int = arg1
  def getArg2: Int = arg2

class Outer:
  class Inner extends Foo(arg2 = 3, defaultValue = { println("default evaluated"); 10 })

@main def Test =
  val outer = new Outer
  val inner = new outer.Inner
  println(inner.getDefault)
  println(inner.getArg1)
  println(inner.getArg2)
