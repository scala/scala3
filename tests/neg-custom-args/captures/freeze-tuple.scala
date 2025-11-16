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

def test1(): Unit =
  val (a, b) = freeze:
    val t1 = allocRef()  // error
    val t2 = allocRef()
    (t1, t2)

def test2(): Unit =
  val a0 = allocRef()
  val b0 = allocRef()
  val (a: Ref^{}, b: Ref^{}) = freeze:  // error // error
    (a0, b0)
