trait A {
  def print: Unit = println("A")
}

trait B extends A {
  override def print: Unit = {
    println("B")
    super.print
  }
}

trait C extends A {
  override def print: Unit = {
    println("C")
    super.print
  }
}

trait D extends B {
  override def print: Unit = {
    println("D")
    super.print
  }
}

trait BB extends B

trait X
trait Y
trait Z

class Test1 extends C with B with BB with D with X with Y with Z:
  override def print: Unit = {
    println("Test 1")
    super.print
  }

class Test2 extends C with B with BB with D with X with Y {
  override def print: Unit = {
    println("Test 2")
    super.print
  }
}

class Test3 extends X with Y with Z with C with B with BB with D {
  override def print: Unit = {
    println("Test 3")
    super.print
  }
}
@main def Test =
  new Test1().print
  new Test2().print
  new Test3().print
