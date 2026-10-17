import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

// The result of an instantiated identity aliases r
def twoRefs(a: Ref^, b: Ref^{any.rd}): Unit = a.set(b.get)
def mutSep() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  twoRefs(id.value[{r}](r), r) // error

// Freezing the result consumes r
def mutConsume() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  val y = id.value[{r}](r)
  val fr = freeze(y)
  r.set(1) // error
