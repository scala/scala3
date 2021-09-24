trait MyTrait:
  def a(): String = ""

class MyClass:
  var myTrait: MyTrait|Null = null

  def printA(): Unit = println(myTrait.nn.a())

@main def runTest(): Unit =
  val mt = new MyTrait:
    override def a(): String = "hello world"

  val mc = MyClass()
  mc.myTrait = mt
  mc.printA()
