package i26000edge

import language.experimental.captureChecking
import caps.*

trait IOCap extends SharedCapability, Classifier
class Console extends IOCap
class Label extends Control
class File
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x
class Box[A](val value: A)

// Capture set parameters under .rd and .only that are later instantiated
def projections(r: Ref^, con: Console^, l: Label^) =
  val s = Ref(1)
  val copy = Box([C^] => (x: Ref^{C}) => (y: Ref^{C.rd}) => x.set(y.get))
  copy.value[{r, s}](r)(s)
  def mkReader[C^](z: Ref^{C}) = Box((y: Ref^{C.rd}) => y.get)
  val g = mkReader[{r}](r)
  g.value(r)
  val f = Box([C^] => (a: Object^{C}) => (b: Object^{C.only[Control]}) => b)
  val _: Object^{l} = f.value[{con, l}](con)(l)

// A capture set parameter that typer instantiates without knowing its instance
def inferredCapSet(lab: Label^): Object^{lab} =
  class Holder[C^](val l: Object^{C}):
    def mk = Box((k: Object^{C}) => k)
  Holder(lab).mk.value(lab)

// Roots in parameter types of lambdas in invariant containers
def roots(r: Ref^) =
  var v = Box((x: Ref^) => x.set(1))
  v = Box((x: Ref^) => x.set(2))
  v.value(r)
  List(Box((x: Ref^) => x.set(1))).head.value(r)

// Parameter types that mention fresh
def withFresh() =
  val a = Box([C^] => (op: () -> Ref^{C, fresh}) => 1)
  val b = Box([C^] => (op: () -> Ref^{C, fresh.rd}) => 1)
  val mk = (op: () => File^{fresh}) => op().hashCode
  val g: (op: () => File^{fresh}) -> Int = mk

// Narrowing the bound of a capture set parameter
def narrow(io: File^) =
  val free = Box([C^] => (y: File^{C}) => y)
  val w: [C^ <: {io}] -> (y: File^{C}) -> File^{y} = free.value

// A parameter type that mentions a local of the right hand side. Under the invariant
// Box, the local cannot be dropped, so the type is given explicitly there.
def withLocal(x: File^) =
  val wl = { val loc: File^ = File(); List([C^] => (y: File^{C, loc}) => y) }
  val _: File^{x} = wl.head[{x}](x)
  val wb: Box[[C^] -> (y: File^{C}) -> File^{y}] = { val loc: File^ = File(); Box([C^] => (y: File^{C, loc}) => y) }
  val _: File^{x} = wb.value[{x}](x)
