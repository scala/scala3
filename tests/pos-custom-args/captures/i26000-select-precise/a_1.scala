package i26000selectprecise
import language.experimental.captureChecking

class File
class Box[A](val value: A)
object io extends File, caps.SharedCapability

object Lib:
  val s1 = Box((x: File^{io}) => 1).value
  val s2 = List((x: File^{io}) => 1).head
  val s3 = ((x: File^{io}) => 1, 2)._1
