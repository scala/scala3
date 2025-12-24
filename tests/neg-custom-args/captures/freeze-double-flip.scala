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
  val magic = freeze:
    (x: Ref^) => (op: Ref^ => IRef) => op(x) // error
  val reallybad: Ref^ -> Ref^{} = x => magic(x)(x => x)
