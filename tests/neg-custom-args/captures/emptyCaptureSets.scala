package p
import language.experimental.captureChecking
import caps.*
import scala.annotation.unchecked.uncheckedCaptures

class File
class FS extends SharedCapability
class Ref extends Mutable:
  private var v = 0
  update def set(x: Int): Unit = v = x
class Box[A](val value: A)

def pure(x: File^) =
  val b = Box(x)
  val c: Box[File] = b // error, the empty capture set of the pure class `Box` is not shown

def capability(fs: FS^): FS^{} = fs // error, `FS` would stand for `FS^`

def mutable(r: Ref^) =
  val r1: Ref^{} = r // error, `Ref^{}` does not allow calls of update methods

def bound[A <: Any^{}](a: A) = a
def useBound(x: File^): File^{x} = bound[File^{x}](x) // error, `Any` as a bound admits capturing types

class Pure
def union(b: Boolean, c0: FS^{}, c1: FS) =
  var v = if b then c0 else Pure()
  v = if b then c1 else Pure() // error, `FS | Pure` would stand for `(FS | Pure)^`

class Svc(val fs: FS):
  def take(x: (File^) @uncheckedCaptures): Unit = ()
def fluid(s: Svc^) =
  val f = s.take
  val g: File -> Unit = f // error, the parameter type of `f` admits all capture sets
