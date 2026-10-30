package i26000rerunescape

import language.experimental.captureChecking
import caps.Mutable

class File
class Box1[A](val value: A)
class Box2[A](val value: A)
class Box3[A](val value: A)
class Box4[A](val value: A)

def withFile[R](op: (f: File^) => R): R = op(File())

// The bound of the closures merged by typer is inferred as the intersection
// of their declared bounds. A later use cannot extend that inferred bound.
// Each case selects the member of another class, so that the member types
// cached for one case do not affect the others.

def scoped(io: File^, io2: File^, b: Boolean): File^{io, io2} =
  withFile: f =>
    val tb = Box1(
      if b then [T <: File^{io}] => (x: T) => { val y: File^{io, io2} = x; y }
      else [T <: File^{io2}] => (x: T) => { val y: File^{io, io2} = x; y })
    tb.value[File^{f}](f) // error

def outer(io: File^, io2: File^, b: Boolean): File^{io, io2} =
  val tb = Box2(
    if b then [T <: File^{io}] => (x: T) => { val y: File^{io, io2} = x; y }
    else [T <: File^{io2}] => (x: T) => { val y: File^{io, io2} = x; y })
  withFile: f =>
    tb.value[File^{f}](f) // error

def elems(io: File^, io2: File^): File^{io, io2} =
  withFile: f =>
    val tb = Box3(List(
      [T <: File^{io}] => (x: T) => { val y: File^{io, io2} = x; y },
      [T <: File^{io2}] => (x: T) => { val y: File^{io, io2} = x; y }))
    tb.value.head[File^{f}](f) // error

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

def capsetParams(r: Ref^, s: Ref^, b: Boolean): Unit =
  val g = Box4(if b then [C^ <: {r}] => (x: Int) => x else [C^ <: {r, s}] => (x: Int) => x + 1)
  g.value[{s}](0) // error
