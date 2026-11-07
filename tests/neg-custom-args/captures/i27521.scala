import language.experimental.separationChecking
import caps.*
class Buf extends Mutable:
  update def put(x: Int): Unit = ()
  def get: Int = 42
class Producer extends Mutable:
  val buf: Buf^ = new Buf
  update def produce(x: Int): Unit = buf.put(x)
def runParallel(op1: () => Unit, op2: () => Unit): Unit = ()

def rootWriteVsPathRead(p: Producer^): Unit =
  val op1: () ->{p} Unit = () => p.produce(2)
  val op2: () ->{p.buf.rd} Unit = () => println(p.buf.get)
  val op3: () ->{p.buf} Unit = () => p.buf.put(42)
  runParallel(op1, op2)  // error
  runParallel(op1, op3)  // error

// An exclusive capability conflicts with any capability it covers, and vice
// versa, unless both are read-only.
def variants(p: Producer^): Unit =
  val wP: () ->{p} Unit = () => p.produce(2)
  val rP: () ->{p.rd} Unit = () => println(p.buf.get)
  val wB: () ->{p.buf} Unit = () => p.buf.put(42)
  val rB: () ->{p.buf.rd} Unit = () => println(p.buf.get)
  runParallel(rB, wP)   // error
  runParallel(rP, wB)   // error
  runParallel(wB, rP)   // error
  runParallel(wP, wP)   // error
  runParallel(rP, rB)   // ok
  runParallel(rB, rB)   // ok
  runParallel(rP, rP)   // ok

def hideWrite(): Unit =
  val p: Producer^ = new Producer
  val x: () => Unit = () => p.produce(2)   // hides p
  println(p.buf.get)    // error

def hideRead(): Unit =
  val p: Producer^ = new Producer
  val y: () => Unit = () => println(p.buf.get)  // hides p.buf.rd
  println(p.buf.get)    // ok
  p.produce(1)          // error
