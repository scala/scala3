import scala.runtime.BoxesRunTime

object Test {
  def main(args: Array[String]): Unit = {
    val boxed: java.lang.Byte = BoxesRunTime.boxToByte(5.toByte)
    assert(boxed == 5.toByte)
    assert(BoxesRunTime.unboxToByte(boxed) == 5.toByte)
  }
}