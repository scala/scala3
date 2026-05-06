import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.{Mutable, freeze}

// A mutable ref and its immutable version
class Ref extends Mutable:
  private var data: Int = 0
  def get: Int = data
  update def set(x: Int): Unit = data = x
def allocRef(): Ref^ = Ref()
type IRef = Ref^{}

// Boxes
case class Box[+T](unbox: T) extends caps.Mutable

// Parallelism
def par(op1: () => Unit, op2: () => Unit): Unit = ()

def test1(): Unit =
  val a = allocRef()
  val xs: Box[Ref^{}] = freeze:
    Box(a)  // error
  val b = xs.unbox
  par(() => a.set(42), () => println(b.get))

def test2(): Unit =
  val a = allocRef()
  val xs = freeze:
    Box(a) // error
  val b = xs.unbox
  par(() => a.set(42), () => println(b.get))  // error

def test3(): Unit =
  val a = allocRef()
  val b = freeze:
    a
  par(() => a.set(42), () => println(b.get)) // error
