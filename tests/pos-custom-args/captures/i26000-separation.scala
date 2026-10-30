import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

def swap(a: Ref^, b: Ref^): Unit = ()
def par(p1: () => Any, p2: () => Any): Unit = ()

// Results for distinct refs are separate
def sepPos1(r: Ref^, s: Ref^) =
  val id = Box([C^] => (a: Ref^{C}) => a)
  swap(id.value[{r}](r), id.value[{s}](s))
  swap(id.value(r), id.value(s)) // C inferred

// Update closures on distinct refs run in parallel
def sepPos2(r: Ref^, s: Ref^) =
  val mk = Box([C^] => (a: Ref^{C}) => () => a.set(1))
  val w: () ->{r} Unit = mk.value[{r}](r)
  par(w, mk.value[{s}](s))

// A read-only instantiation runs in parallel with readers
def sepPos3(r: Ref^) =
  val reader = Box([C^] => (a: Ref^{C}) => () => a.get)
  val g: () ->{r.rd} Int = reader.value[{r.rd}](r)
  par(g, () => r.get)

// The same ref for two parameters of the same capture set
def sepPos4(r: Ref^, s: Ref^) =
  val f = Box([C^] => (a: Ref^{C}) => (b: Ref^{C}) => () => a.set(b.get))
  val fr: () ->{r} Unit = f.value[{r}](r)(r)
  par(fr, f.value[{s}](s)(s))

// Outer write with a read-only argument
def sepPos5(r: Ref^, s: Ref^) =
  val f = Box([C^] => (a: Ref^{C}) => s.set(a.get))
  par(() => f.value[{r.rd}](r), () => r.get)

// Two capture set parameters, exclusive and read-only
def sepPos6(r: Ref^, s: Ref^) =
  val f = Box([C^, D^] => (a: Ref^{C}) => (b: Ref^{D}) => () => a.set(b.get))
  val w: () ->{r, s.rd} Unit = f.value[{r}, {s.rd}](r)(s)
  par(w, () => s.get)

// Exclusive bound, read-only instantiation
def sepPos7(r: Ref^) =
  val b = Box([C^ <: {r}] => (a: Ref^{C}) => a)
  val ro: Ref^{r.rd} = b.value[{r.rd}](r)
  par(() => ro.get, () => r.get)
