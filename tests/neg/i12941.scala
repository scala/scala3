object A:
  def myFun(op: String ?=> Unit) = ()

@main def func: Unit =
  A.myFun {
    val res: String = summon[String]
    println(ress)  // error
  }

class I:
  def runSth: Int = 1

abstract class A:
  def myFun(op: I ?=> Unit) =
    op(using I())
    1

class B extends A

def assertEquals(x: String, y: Int, z: Int): Unit = ()

@main def hello: Unit =

  B().myFun {
    val res = summon[I].runSth
    assertEquals("", 1, res, "asd") // error
    println("Hello!")
  }