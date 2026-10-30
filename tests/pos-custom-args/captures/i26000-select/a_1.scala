package i26000select
import language.experimental.captureChecking

class File
class Box[A](val value: A)
class CBox[+A](val value: A)
object io extends File, caps.SharedCapability
object y extends File, caps.SharedCapability

object Lib:
  val g = (x: File^{io}) => 1
  val h = (x: File^{y}) => 2
  val gb = Box((x: File^{io}) => 1)
  val hb = Box((x: File^{y}) => 2)
  val gc = CBox((x: File^{io}) => 1)
  val hc = CBox((x: File^{y}) => 2)
  val gt = ((x: File^{io}) => 1, 0)
  val ht = ((x: File^{y}) => 2, 0)
