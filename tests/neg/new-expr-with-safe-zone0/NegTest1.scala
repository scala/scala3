import language.experimental.captureChecking
import scala.scalanative.safe.{SafeZone, MemorySafeZone}

object NegTest1 {
  def test(): Unit = {
    import scala.scalanative.safe.MemorySafeZone
    val sz: {*} SafeZone = new MemorySafeZone("test-handle")

    val x0 = new {sz} { if 1 > 0 then A(0) else A(1) } // error: syntax error
    val x1 = new {sz} A(0) with B // error: syntax error
    val x2 = new {sz} A(0) with B { // error: syntax error
      override def sayHi(): String = s"Hi from overrided"
    }
  }
}