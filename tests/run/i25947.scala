// https://github.com/scala/scala3/issues/25947
import scala.language.reflectiveCalls

class Holder {
  def nextBytes(bytes: Array[Byte]): Unit =
    var i = 0
    while i < bytes.length do
      bytes(i) = (i + 1).toByte
      i += 1
}

object Test {
  type WithNextBytes =
    AnyRef { def nextBytes(bytes: Array[Byte]): Unit }

  def call(value: AnyRef, bytes: Array[Byte]): Unit =
    value.asInstanceOf[WithNextBytes].nextBytes(bytes)

  def main(args: Array[String]): Unit =
    val bs = new Array[Byte](3)
    call(new Holder, bs)
    assert(bs.toList == List[Byte](1, 2, 3), bs.toList)
}
