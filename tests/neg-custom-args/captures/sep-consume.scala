import language.experimental.captureChecking
import caps.*

class Ref extends Mutable:
  private var _data = 0
  def get: Int = _data
  mut def put(x: Int): Unit = _data = x

// require f and g to be non-interfering
def par(f: () => Unit, g: () => Unit): Unit = ???

def bad(@consume op: () ->{cap.rd} Unit): () => Unit = op

def test2(@consume x: Ref^): Unit =
  val f: () ->{x.rd} Unit = () => x.get
  val rx: () => Unit = bad(f)  // hides x.rd in the resulting `cap`
  x.put(42)  // error
  x.get      // error
  par(rx, () => x.put(42))  // error
  par(rx, () => x.get)  // error

def test3(@consume x: Ref^): Unit =
  val f: () ->{x.rd} Unit = () => x.get
  def foo = bad(f) // error
  foo()
  foo()
