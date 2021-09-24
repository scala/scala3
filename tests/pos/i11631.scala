trait MyTrait:
  def a(): String = ""

class Nulll

extension [T](x: T | Nulll) inline def nnn: x.type & T = ???

class MyClass:
  var myTrait: MyTrait|Null = null

  def printA(): Unit = println(myTrait.nnn.a())

@main def runTest(): Unit =
  val mt = new MyTrait:
    override def a(): String = "hello world"

  val mc = MyClass()
  mc.myTrait = mt
  mc.printA()
