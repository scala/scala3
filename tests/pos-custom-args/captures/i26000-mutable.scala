import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

// Update method on a parameter of type Ref^{C}
def mutSetter() =
  val r = Ref(0)
  val setter = Box([C^] => (x: Ref^{C}) => x.set(1))
  setter.value[{r}](r)

// Read-only instantiation and bounds
def mutReadOnly() =
  val r = Ref(0)
  val getter = Box([C^] => (x: Ref^{C}) => x.get)
  val _: Int = getter.value[{r.rd}](r)
  val bounded = Box([C^ <: {r.rd}] => (x: Ref^{C}) => x.get)
  val _: Int = bounded.value[{r.rd}](r)
  val rootBounded = Box([C^ <: {any.rd}] => (x: Ref^{C}) => x.get)
  val _: Int = rootBounded.value[{r.rd}](r)

// Parameter types with C.rd
def mutCRd() =
  val r = Ref(0)
  val s = Ref(1)
  val ro = Box([C^] => (x: Ref^{C.rd}) => x.get)
  val _: Int = ro.value[{r}](r)
  val copy = Box([C^] => (x: Ref^{C}) => (y: Ref^{C.rd}) => x.set(y.get))
  copy.value[{r, s}](r)(s)

// Curried lambda returning a mutating thunk
def mutThunk() =
  val r = Ref(0)
  val mk = Box([C^] => (a: Ref^{C}) => () => a.set(a.get + 1))
  val inc: () ->{r} Unit = mk.value[{r}](r)
  inc()

// An instantiated identity keeps exclusivity
def mutIdentity() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  val r2: Ref^{r} = id.value[{r}](r)
  r2.set(3)

// Vars and lambda values
def mutVar() =
  val r = Ref(0)
  var v = Box([C^] => (x: Ref^{C}) => x.set(0))
  v = Box([C^] => (x: Ref^{C}) => x.set(1))
  v.value[{r}](r)
  val p = [C^] => (x: Ref^{C}) => x.set(2)
  val b = Box(p)
  b.value[{r}](r)

// Read-only C with an outer update
def mutOuter() =
  val total = Ref(0)
  val r = Ref(5)
  val acc = Box([C^] => (x: Ref^{C}) => total.set(total.get + x.get))
  acc.value[{r.rd}](r)
  val b = Box([C^ <: {r}] => (x: Ref^{C}) => x.set(1))
  b.value[{r}](r)

// Freezing the result of an instantiated identity
def mutFreeze() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  val y = id.value[{r}](r)
  y.set(1)
  val fr = freeze(y)
  val _: Ref^{} = fr

