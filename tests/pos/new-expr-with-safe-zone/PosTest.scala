import language.experimental.captureChecking
import scala.scalanative.safe.{SafeZone, MemorySafeZone}

object PosTest {
  class A(v0: Int = 0) {
    override def toString(): String = s"{$v0}"
  }

  trait B {
    def sayHi(): String = s"Hi from B"
  }

  def WithoutSafeZone(): Unit = {
    val x0 = new { if 1 > 0 then A(0) else A(1) }
    val x1 = new A(0) with B
    assert(x1.sayHi().equals("Hi from B"))
    val x2 = new A(0) with B {
      override def sayHi(): String = s"Hi from overrided"
    }
    assert(x2.sayHi().equals("Hi from overrided"))
  }

  def WithSafeZone0(): Unit = {
    val sz: {*} MemorySafeZone = new MemorySafeZone("test-handle")
    val a0 = new {sz} A(0)
  }

  def WithSafeZone1(): Unit = {
    val sz: {*} SafeZone = new MemorySafeZone("test-handle")
    val a0: {sz} A = new {sz} A(0)
  }
}