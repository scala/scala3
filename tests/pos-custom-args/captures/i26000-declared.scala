package i26000declared

import language.experimental.captureChecking
import caps.*

class File
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x
class Box[A](val value: A)
object io extends File, SharedCapability

// Public definitions: their inferred types are seen by other compilation units,
// with the parameter types and bounds of lambdas as written
object Lib:
  val g = Box((x: File^{io}) => 1)
  val h = Box([C^ >: {io}] => (x: File^{C}) => 1)
  val k = Box((op: File^{io} => Unit, f: File^{io}) => op(f))
  val m = Box((r: Ref^) => r.set(1))
  val l = List((x: File^{io}) => 1, (x: File^{io}) => 2)

  def use(r: Ref^) =
    g.value(io)
    h.value[{io}](io)
    k.value((f: File^{io}) => (), io)
    m.value(r)
    l.head(io)

object Roots:
  def take(b: Box[(x: File^) -> Unit]) = ()
  def root() = take(Box((x: File^) => ()))
