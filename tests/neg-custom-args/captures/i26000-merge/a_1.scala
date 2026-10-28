package i26000merge

import language.experimental.captureChecking

class File
class Box[A](val value: A)
object a extends File, caps.SharedCapability
object y extends File, caps.SharedCapability
object z extends File, caps.SharedCapability

object Lib:
  def flag: Boolean = true
  val m = Box(if flag then (x: File^{a, y}) => () else (x: File^{a, z}) => ())
  val mp = Box(if flag then [C^] => (x: File^{C, a, y}) => () else [C^] => (x: File^{C, a, z}) => ())
  val mhi = Box(if flag then [C^ <: {a}] => (x: File^{C}) => () else [C^ <: {a, y}] => (x: File^{C}) => ())
  val mlo = Box(if flag then [C^ >: {a}] => (x: File^{C}) => () else [C^ >: {y}] => (x: File^{C}) => ())
  val w = { val loc: File^ = File(); Box([C^] => (x: File^{C, loc}) => 1) }
