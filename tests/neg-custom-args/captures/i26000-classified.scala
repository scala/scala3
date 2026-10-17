import caps.*

trait IOCap extends SharedCapability, Classifier
class Console extends IOCap:
  def print(): Unit = ()
class Label extends Control:
  def break(): Unit = ()

class Box[A](val value: A)

class Try[+T]
object Try:
  def apply[T, C^](body: () ->{C} T): Try[T]^{C.only[Control]} = ???

// A root bound with a classifier is enforced
def clsBoundNeg(l: Label^) =
  val f = Box([C^ <: {any.only[IOCap]}] => (x: Object^{C}) => x)
  f.value[{l}](l) // error

// Bounds are enforced even if C is unused in the parameters
def clsBoundOnlyNeg(con: Console^) =
  val f = Box([C^ <: {any.only[Unscoped]}] => (x: Int) => x)
  val _: Int = f.value[{con}](0) // error

// `c` is not below `c.only[IOCap]`
def clsBoundPathNeg(c: Object^) =
  val f = Box([C^ <: {c.only[IOCap]}] => (x: Object^{C}) => x)
  f.value[{c}](c) // error

// `C.only[Control]` rejects the IOCap part of C
def clsOnlyNeg(con: Console^, l: Label^) =
  val f = Box([C^] => (x: Object^{C.only[Control]}) => x)
  f.value[{con, l}](con) // error
  val _: Object^{con} = f.value[{con, l}](l) // error

// Higher-order parameters, with a restricted result or a classifier bound
def clsTryNeg(con: Console^, l: Label^) =
  val f = Box([C^] => (body: () ->{C} Unit) => Try(body))
  val _: Try[Unit]^{con} = f.value[{con, l}](() => { con.print(); l.break() }) // error
  val g = Box([C^ <: {any.only[Control]}] => (body: () ->{C} Unit) => body())
  g.value[{con}](() => ()) // error
