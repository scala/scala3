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

object Merges:
  def take(b: Box[(x: File^) -> Unit]) = ()

  def merged(a: File^, y: File^, z: File^, c: Boolean) =
    val f1 = (x: File^{a, y}) => ()
    val f2 = (x: File^{a, z}) => ()
    // The merged type of the two boxes is inferred, the types of the boxes are kept
    val b = if c then Box(f1) else Box(f2)
    val _: File^{a} => Unit = b.value
    val bs = List(Box(f1), Box(f2))
    val _ = bs.head.value(a)
    // A merge through a type variable
    val m = Box(if c then f1 else f2)
    m.value(a)
    take(Box((x: File^) => ()))
