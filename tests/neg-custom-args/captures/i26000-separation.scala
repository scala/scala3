package i26000separation

import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

def swap(a: Ref^, b: Ref^): Unit = ()
def take(consume x: Ref^): Unit = ()
def par(p1: () => Unit, p2: () => Unit): Unit = ()

// Results of an instantiated identity alias
def sepNeg1(r: Ref^) =
  val id = Box([C^] => (a: Ref^{C}) => a)
  swap(id.value[{r}](r), id.value[{r}](r)) // error
  swap(id.value(r), r) // error

// Update closures for the same ref interfere
def sepNeg2(r: Ref^) =
  val mk = Box([C^] => (a: Ref^{C}) => () => a.set(1))
  par(mk.value[{r}](r), mk.value[{r}](r)) // error
  par(mk.value[{r}](r), () => r.set(2)) // error

// A result aliasing a consumed ref is no longer available
def sepNeg3(consume r: Ref^) =
  val id = Box([C^] => (a: Ref^{C}) => a)
  val y = id.value[{r}](r)
  take(r)
  y.set(1) // error

// Parameters of the same capture set may alias
def sepNeg4() =
  val f = Box([C^] => (a: Ref^{C}) => (b: Ref^{C}) => swap(a, b)) // error

// Interference through the argument and through a write to an outer ref
def sepNeg5(r: Ref^, s: Ref^) =
  val f = Box([C^] => (a: Ref^{C}) => s.set(a.get))
  par(() => f.value[{r}](r), () => r.set(0)) // error
  par(() => f.value[{r.rd}](r), () => s.set(2)) // error

// The closure of a curried lambda captures both arguments
def sepNeg6(r: Ref^, s: Ref^) =
  val f = Box([C^] => (a: Ref^{C}) => (b: Ref^{C}) => () => a.set(b.get))
  par(f.value[{r}](r)(r), f.value[{r, s}](s)(r)) // error
  val g = f.value[{r, s}](r)
  par(g(s), () => s.set(0)) // error
