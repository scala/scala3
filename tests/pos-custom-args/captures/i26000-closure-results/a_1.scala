package i26000closureresults

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object io extends File, caps.SharedCapability
object jo extends File, caps.SharedCapability

object Lib:
  val m = (y: File^{io}) => 1
object Lib2:
  val m2 = (y: File^{jo}) => 2
