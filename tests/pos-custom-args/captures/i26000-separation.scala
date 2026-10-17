import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Results for distinct refs are separate
object SepPos1:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def swap(a: Ref^, b: Ref^): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val id = Box([C^] => (a: Ref^{C}) => a)
    swap(id.value[{r}](r), id.value[{s}](s))
    swap(id.value(r), id.value(s)) // C inferred
    id.value[{r}](r).put(1)

// Update closures on distinct refs run in parallel
object SepPos2:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val mk = Box([C^] => (a: Ref^{C}) => () => a.put(1))
    val w: () ->{r} Unit = mk.value[{r}](r)
    par(w, mk.value[{s}](s))

// A read-only instantiation runs in parallel with readers
object SepPos3:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Int, p2: () => Int): Unit = ()

  def test(r: Ref^) =
    val reader = Box([C^] => (a: Ref^{C}) => () => a.get)
    val g: () ->{r.rd} Int = reader.value[{r.rd}](r)
    par(g, g)
    par(g, () => r.get)

// The same ref for two parameters of the same capture set
object SepPos4:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val f = Box([C^] => (a: Ref^{C}) => (b: Ref^{C}) => () => a.put(b.get))
    val fr: () ->{r} Unit = f.value[{r}](r)(r)
    par(fr, f.value[{s}](s)(s))

// A lambda value in a Box and in an Option
object SepPos5:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val p = [C^] => (a: Ref^{C}) => () => a.put(1)
    val g = Box(p)
    val o = Some(p)
    par(g.value[{r}](r), o.get[{s}](s))

// Outer write, read-only argument
object SepPos6:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Int): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val f = Box([C^] => (a: Ref^{C}) => s.put(a.get))
    par(() => f.value[{r.rd}](r), () => r.get)

// Two capture set parameters, exclusive and read-only
object SepPos7:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val f = Box([C^, D^] => (a: Ref^{C}) => (b: Ref^{D}) => () => a.put(b.get))
    val w: () ->{r, s.rd} Unit = f.value[{r}, {s.rd}](r)(s)
    par(w, () => { s.get; () })

// Exclusive bound, read-only instantiation
object SepPos8:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Int, p2: () => Int): Unit = ()

  def test(r: Ref^) =
    val b = Box([C^ <: {r}] => (a: Ref^{C}) => a)
    val ro: Ref^{r.rd} = b.value[{r.rd}](r)
    par(() => ro.get, () => r.get)

