import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Results of an instantiated identity alias
object SepNeg1:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def swap(a: Ref^, b: Ref^): Unit = ()

  def test(r: Ref^) =
    val id = Box([C^] => (a: Ref^{C}) => a)
    swap(id.value[{r}](r), id.value[{r}](r)) // error
    swap(id.value(r), r) // error

// Update closures for the same ref interfere
object SepNeg2:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^) =
    val mk = Box([C^] => (a: Ref^{C}) => () => a.put(1))
    par(mk.value[{r}](r), mk.value[{r}](r)) // error
    par(mk.value[{r}](r), () => r.put(2)) // error

// Consumed refs
object SepNeg3:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def take(consume x: Ref^): Unit = ()

  def test(consume r: Ref^) =
    val id = Box([C^] => (a: Ref^{C}) => a)
    val y = id.value[{r}](r)
    take(r)
    y.put(1) // error
    id.value[{r}](r) // error

// Parameters of the same capture set may alias
object SepNeg5:
  class Ref extends Mutable:
    var x = 0
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def swap(a: Ref^, b: Ref^): Unit = ()
  def take(consume x: Ref^): Unit = ()

  def test() =
    val f = Box([C^] => (a: Ref^{C}) => (b: Ref^{C}) => swap(a, b)) // error
    val g = Box([C^] => (a: Ref^{C}) => take(a)) // error

// Writes to an outer ref interfere
object SepNeg6:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val f = Box([C^] => (a: Ref^{C}) => s.put(a.get))
    par(() => f.value[{r}](r), () => r.put(0)) // error
    par(() => f.value[{r.rd}](r), () => s.put(2)) // error

// The closure of a curried lambda captures both arguments
object SepNeg7:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def par(p1: () => Unit, p2: () => Unit): Unit = ()

  def test(r: Ref^, s: Ref^) =
    val f = Box([C^] => (a: Ref^{C}) => (b: Ref^{C}) => () => a.put(b.get))
    par(f.value[{r}](r)(r), f.value[{r, s}](s)(r)) // error
    val g = f.value[{r, s}](r)
    par(g(s), () => s.put(0)) // error

