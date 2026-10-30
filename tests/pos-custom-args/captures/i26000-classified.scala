package i26000classified

import caps.*

trait IOCap extends SharedCapability, Classifier
class Console extends IOCap:
  def print(): Unit = ()
class Label extends Control:
  def break(): Unit = ()

class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x

class Box[A](val value: A)

class Try[+T]
object Try:
  def apply[T, C^](body: () ->{C} T): Try[T]^{C.only[Control]} = ???

// A root bound with a classifier
def clsBoundAny(con: Console^) =
  val f = Box([C^ <: {any.only[IOCap]}] => (x: Console^{C}) => x)
  val _: Console^{con} = f.value[{con}](con)

// A bound with a restricted capability
def clsBoundPath(c: Object^, y: Object^{c.only[IOCap]}) =
  val f = Box([C^ <: {c.only[IOCap]}] => (x: Object^{C}) => x)
  val _: Object^{c.only[IOCap]} = f.value[{c.only[IOCap]}](y)

// `C.only` and `C.except` in parameter types
def clsOnlyExcept(con: Console^, l: Label^) =
  val f = Box([C^] => (x: Object^{C.only[Control]}) => x)
  val _: Object^{l} = f.value[{con, l}](l)
  val g = Box([C^] => (a: Object^{C}) => (b: Object^{C.except[Control]}) => b)
  val _: Object^{con} = g.value[{con, l}](l)(con)

// A lambda nested in two containers
def clsNested(l: Label^) =
  val f = Box(Some([C^ <: {any.only[Control]}] => (x: Label^{C}) => x))
  val _: Label^{l} = f.value.get[{l}](l)

// A lambda value with a classifier bound, through `identity`
def clsPolyValue(con: Console^, l: Label^) =
  val p = [C^ <: {any.only[SharedCapability]}] => (x: Object^{C.only[IOCap]}) => x
  val f = identity(Box(p))
  val _: Object^{con} = f.value[{con, l}](con)

// Higher-order parameters, with a restricted result or a classifier bound
def clsTry(con: Console^, l: Label^) =
  val f = Box([C^] => (body: () ->{C} Unit) => Try(body))
  val _: Try[Unit]^{l} = f.value[{con, l}](() => { con.print(); l.break() })
  val g = Box([C^ <: {any.only[Control]}] => (body: () ->{C} Unit) => body())
  g.value[{l}](() => l.break())

// An `Unscoped` bound on an updated Mutable parameter
def clsUnscoped(r: Ref^) =
  val setter = Box([C^ <: {any.only[Unscoped]}] => (x: Ref^{C}) => x.set(1))
  setter.value[{r}](r)
  r.set(2)

// C mixed with a restricted outer capability
def clsMixed(l: Label^, c: Object^, y: Object^{c.only[Control]}) =
  val f = Box([C^] => (x: Object^{C, c.only[Control]}) => x)
  val _: Object^{l, c.only[Control]} = f.value[{l}](y)
