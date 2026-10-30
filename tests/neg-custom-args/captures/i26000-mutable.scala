package i26000mutable

import caps.*

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

// Read-only bounds are enforced
def mutRoBound() =
  val r = Ref(0)
  val bounded = Box([C^ <: {r.rd}] => (x: Ref^{C}) => x.get)
  bounded.value[{r}](r) // error
  val rootBounded = Box([C^ <: {any.rd}] => (x: Ref^{C}) => x.get)
  rootBounded.value[{r}](r) // error

// A parameter of type Ref^{C.rd} gives a read-only result
def mutCRdNeg() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C.rd}) => x)
  val r2 = id.value[{r}](r)
  r2.set(3) // error

// A read-only instantiation gives a read-only result
def mutRoInst() =
  val r = Ref(0)
  val id = Box([C^] => (x: Ref^{C}) => x)
  val r2 = id.value[{r.rd}](r)
  r2.set(1) // error

// A frozen ref cannot be passed to a parameter of type Ref^
def mutFrozen() =
  val s = freeze(Ref(0))
  val g = Box((b: Ref^) => b.set(1))
  g.value(s) // error
