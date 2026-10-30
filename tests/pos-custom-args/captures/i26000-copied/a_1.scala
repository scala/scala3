package i26000copied

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object io extends File, caps.SharedCapability

object Lib:
  val m = (y: File^{io}) => 1
  val bm = Box((y: File^{io}) => 1)
