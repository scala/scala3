package p
import language.experimental.captureChecking
import caps.*

class File
class Box[A](val value: A)
case class CBox[A](value: A)
class Same[A](val a: A, val b: A)

def poly(x: File^) =
  val f = Box([C^] => (f: File^{C}) => f)
  val r: File^{x} = f.value[{x}](x)

def curried(x: File^) =
  val f = Box([C^] => (f: File^{C}) => (h: File^{C}) => (g: File^{C}) => { println(h); (f, h) })
  val r: (File^{x}, File^{x}) = f.value[{x}](x)(x)(x)

def mono(x: File^) =
  val b = Box((f: File^{x}) => ())
  b.value(x)

def capParam(x: File^) =
  val b = Box((f: File^) => ())
  b.value(x)

class Containers:
  private val l = List([C^] => (f: File^{C}) => f)
  private val o = Option([C^] => (f: File^{C}) => f)
  private val c = CBox([C^] => (f: File^{C}) => f)
  def use(x: File^) =
    val r1: File^{x} = l.head[{x}](x)
    val r2: File^{x} = o.get[{x}](x)
    val r3: File^{x} = c.value[{x}](x)

def bounded(a: File^, x: File^) =
  val f = Box([C^ <: {a}] => (f: File^{C}) => f)
  val g = Box([C^, D^ <: {C}] => (g: File^{D}) => (g: File^{C}))
  val r1: File^{a} = f.value[{a}](a)
  val r2: File^{x} = g.value[{x}, {x}](x)
  val h = Box([T <: () -> File^{a}] => (k: T) => k())
  val r3: File^{a} = h.value[() -> File^{a}](() => a)

def alias(x: File^) =
  val c = CBox([C^] => (f: File^{C}) => f)
  val c2 = c
  val r: File^{x} = c2.value[{x}](x)

def localDef(x: File^) =
  def mk(y: File^) = Box((f: File^{y}) => ())
  mk(x).value(x)

def localPolyDef(x: File^) =
  def mk() = Box([C^] => (f: File^{C}) => f)
  val b = mk()
  val r: File^{x} = mk().value[{x}](x)

class Sh extends SharedCapability

def classified(s: Sh^) =
  val b = Box([C^] => (r: Sh^{C}) => ())
  b.value[{s}](s)

// The bound lets `C` flow into the classified formal `Sh^{C}`
def classifiedBounded(s: Sh^) =
  val b: Box[[C^ <: {any.only[SharedCapability]}] -> (r: Sh^{C}) -> Sh^{C}] =
    Box([C^ <: {any.only[SharedCapability]}] => (r: Sh^{C}) => r)
  val r: Sh^{s} = b.value[{s}](s)

class K:
  private val f = Box([C^] => (f: File^{C}) => f)
  def use(x: File^): File^{x} = f.value[{x}](x)

// Type arguments not determined by a single closure stay inferred
class Sub[A, B <: A](val a: A, val b: B)

def lubs(x: File^, y: File^, c: Boolean) =
  val l = List((f: File^{x}) => (), (f: File^{y}) => ())
  val b = Box(if c then (f: File^{x}) => () else (f: File^{y}) => ())
  val s = Same((f: File^{x}) => (), (f: File^{y}) => ())
  val t = Sub((f: File^{x}) => (), (f: File^{y}) => ())

// Right hand sides typed against an expected type keep inferring type arguments
def expected(x: File^) =
  def take(g: Int => Box[File -> Unit]) = ()
  take(i => Box((f: File^{x}) => ()))
  abstract class T:
    def b: Box[File -> Unit]
  class C extends T:
    override def b = Box((f: File^{x}) => ())
    def m(b: Box[File -> Unit] = Box((f: File^{x}) => ())) = ()

def mkBox[A](a: A): Box[A] = Box(a)
def take(a: Int, b: Box[File -> Unit]) = ()
def takeC(b: Box[File -> Unit])(n: Int = 0) = ()

def lifted(x: File^) =
  take(b = mkBox((f: File^{x}) => ()), a = 1)
  takeC(Box((f: File^{x}) => ()))()
