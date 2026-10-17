import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

// Read-only bounds are kept
def mutRoBound() =
  val r = Ref(0)
  val bad = Box([C^ <: {r.rd}] => (x: Ref^{C}) => x.set(1)) // error
  val bounded = Box([C^ <: {r.rd}] => (x: Ref^{C}) => x.get)
  bounded.value[{r}](r) // error
  val rootBounded = Box([C^ <: {any.rd}] => (x: Ref^{C}) => x.get)
  rootBounded.value[{r}](r) // error

// Parameters typed with C.rd are read-only
def mutCRdNeg() =
  val r = Ref(0)
  val bad = Box([C^] => (x: Ref^{C.rd}) => x.set(1)) // error
  val id = Box([C^] => (x: Ref^{C.rd}) => x)
  val r2 = id.value[{r}](r)
  r2.set(3) // error

// A read-only instantiation gives a read-only result
def mutRoInst() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  val r2 = id.value[{r.rd}](r)
  r2.set(1) // error

// The var keeps its precise type
def mutVarNeg() =
  val r = Ref(0)
  var v = Box([C^] => (x: Ref^{C}) => x.set(0))
  v = Box([C^] => (x: Ref^{C}) => { r.set(0); x.set(1) }) // error

// Stateful parameters keep their root, so frozen refs are rejected
def mutFrozen() =
  val s = freeze(Ref(0))
  val g = Box((b: Ref^) => b.set(1))
  g.value(s) // error
