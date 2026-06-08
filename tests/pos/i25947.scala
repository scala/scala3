// https://github.com/scala/scala3/issues/25947
import scala.language.reflectiveCalls

object Repro {
  private type WithNextBytes =
    AnyRef { def nextBytes(bytes: Array[Byte]): Unit }

  def call(value: AnyRef, bytes: Array[Byte]): Unit =
    value.asInstanceOf[WithNextBytes].nextBytes(bytes)
}
