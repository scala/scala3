package i26000mergeedge

import language.experimental.captureChecking

class File
trait Tag
class Box[A](val value: A)
object a extends File, Tag, caps.SharedCapability
object y extends File, Tag, caps.SharedCapability
object z extends File, Tag, caps.SharedCapability

object Lib:
  def flag: Boolean = true
  val mu = Box(if flag then (x: File^{a, y} | Int) => () else (x: File^{a, z} | Int) => ())
  val mi = Box(if flag then (x: File^{a, y} & Tag) => () else (x: File^{a, z} & Tag) => ())
  val mn = Box(if flag then (x: File^{a, y}) => () else (f: File^{a, z}) => ())
  val f1 = (x: File^{a, y}) => ()
  val ms = Box(if flag then f1 else (x: File^{a, z}) => ())

object Use uses y:
  def misuse() =
    Lib.mu.value(y) // error
    Lib.mn.value(y) // error
    Lib.ms.value(y) // error
