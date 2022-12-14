import language.experimental.captureChecking
import scala.scalanative.safe.{SafeZone, MemorySafeZone}

object NegTest0 {
  def test(): Unit = {
    val a0  = new {} A(0) // error: syntax error
    val str = "abc"
    val a1 = new {str} A(0) // error: type match error
  }
}