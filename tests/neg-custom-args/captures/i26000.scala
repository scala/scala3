package p
import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)

def leak(x: File^) =
  val f = Box([C^] => (f: File^{C}) => f)
  val l: File = f.value[{x}](x) // error

def arrow(x: File^, y: File^) =
  val b = Box((f: File^{x}) => y)
  val g: File^{x} -> File^{y} = b.value // error

def wrongArg(x: File^, y: File^) =
  val b = Box((f: File^{x}) => ())
  b.value(y) // error

// Bounds of capture parameters are kept in the type of `f`
def upper(a: File^, b: File^) =
  val f = Box([C^ <: {a}] => (f: File^{C}) => (f: File^{a}))
  val r: File^{a} = f.value[{b}](b) // error

def lower(a: File^) =
  val f = Box([C^ >: {a}] => (g: File^{a}) => (g: File^{C}))
  val r: File = f.value[{}](a) // error

def typeParam(a: File^, b: File^) =
  val f = Box([T <: File^{a}] => (x: T) => (x: File^{a}))
  val r: File^{a} = f.value[File^{b}](b) // error

// The bounds of a polymorphic function returned by the closure's body are kept
def bodyBounds(x: File^) =
  val f = Box([C^] => (f: File^{C}) => { println(); [D^ >: {C}] => (v: Unit) => (f: File^{D}) })
  val r: File = f.value[{x}](x)[{}](()) // error

class Sh extends SharedCapability

// The closures return their argument, so their results are not pure
def classifiedLeak(s: Sh^): Sh^{} =
  val b: Box[[C^] -> (r: Sh^{C}) -> Sh^{}] = Box([C^] => (r: Sh^{C}) => r) // error
  b.value[{s}](s)

def classifiedBoundedLeak(s: Sh^): Sh^{} =
  val b: Box[[C^ <: {any.only[SharedCapability]}] -> (r: Sh^{C}) -> Sh^{}] =
    Box([C^ <: {any.only[SharedCapability]}] => (r: Sh^{C}) => r) // error
  b.value[{s}](s)
