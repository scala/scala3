import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

// Updates through a parameter of type Ref^{C}, with and without an exclusive bound
def mutSetter() =
  val r = Ref(0)
  val setter = Box([C^] => (x: Ref^{C}) => x.set(1))
  setter.value[{r}](r)
  val bounded = Box([C^ <: {r}] => (x: Ref^{C}) => x.set(1))
  bounded.value[{r}](r)
  val exclusive = Box((x: Ref^) => x.set(1))
  exclusive.value(r)
  r.set(2)

// Read-only instantiations and read-only bounds
def mutReadOnly() =
  val r = Ref(0)
  val getter = Box([C^] => (x: Ref^{C}) => x.get)
  val _: Int = getter.value[{r.rd}](r)
  val bounded = Box([C^ <: {r.rd}] => (x: Ref^{C}) => x.get)
  val _: Int = bounded.value[{r.rd}](r)
  val rootBounded = Box([C^ <: {any.rd}] => (x: Ref^{C}) => x.get)
  val _: Int = rootBounded.value[{r.rd}](r)

// Parameter types with C and C.rd
def mutCRd() =
  val r = Ref(0)
  val s = Ref(1)
  val copy = Box([C^] => (x: Ref^{C}) => (y: Ref^{C.rd}) => x.set(y.get))
  copy.value[{r, s}](r)(s)

// A reassigned var
def mutVar() =
  val r = Ref(0)
  var v = Box([C^] => (x: Ref^{C}) => x.set(0))
  v = Box([C^] => (x: Ref^{C}) => x.set(1))
  v.value[{r}](r)

// An instantiated identity keeps exclusivity, and its result can be frozen
def mutFreeze() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  val y = id.value[{r}](r)
  y.set(1)
  val fr = freeze(y)
  val _: Ref^{} = fr
