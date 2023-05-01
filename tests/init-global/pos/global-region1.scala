import scala.annotation.init

trait B { def foo(): Int }
class C(var x: Int) extends B { def foo(): Int = 20 }
class D(var y: Int) extends B { def foo(): Int = A.m }
class Box(var value: B)

object A:
  val box1: Box = new Box(new C(5)): @init.region
  val box2: Box = new Box(new D(10)): @init.region
  val m: Int = box1.value.foo()                      // ok
