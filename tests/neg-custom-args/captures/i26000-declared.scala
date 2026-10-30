package i26000declared

import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)
object io extends File, SharedCapability

object Lib:
  val g = Box((x: File^{io}) => 1)
  val h = Box([C^ >: {io}] => (x: File^{C}) => 1)
  val u = Box([C^ <: {io}] => (x: File^{C}) => 1)

  def misuse(y: File^) =
    g.value(y) // error
    h.value[{y}](y) // error
    u.value[{y}](y) // error

object Merges:
  def merged(a: File^, y: File^, z: File^, c: Boolean) =
    val f1 = (x: File^{a, y}) => ()
    val f2 = (x: File^{a, z}) => ()
    val m = Box(if c then f1 else f2)
    m.value(y) // error
