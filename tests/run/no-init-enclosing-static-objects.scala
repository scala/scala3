object Enclosing1 {
  println("must not be executed")

  object InnerObject {
    val foo: Int = 42
    println("InnerObject constructor")
  }

  class InnerClass(val foo: Int) {
    println("InnerClass constructor")
  }
}

object Enclosing2 {
  println("Enclosing2 constructor")

  object InnerObject {
    val foo: Int = 65
    println("Enclosing2.InnerObject constructor")
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    testExplicit()
    testThroughImport()
    testThroughFunction()
  }

  def testExplicit(): Unit = {
    println(Enclosing1.InnerObject.foo)
    println(new Enclosing1.InnerClass(5).foo)
  }

  def testThroughImport(): Unit = {
    import Enclosing1.*
    println(InnerObject.foo)
    println(new InnerClass(5).foo)
  }

  def testThroughFunction(): Unit = {
    println(getEnclosing2().InnerObject.foo)
  }

  def getEnclosing2(): Enclosing2.type = {
    println("getEnclosing2()")
    Enclosing2
  }
}
