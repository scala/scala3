import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Classifier bound `any.only[IOCap]`
object ClsBoundAny:
  trait IOCap extends SharedCapability, Classifier
  class Console extends IOCap
  class Box[A](val value: A)
  def test(con: Console^) =
    val f = Box([C^ <: {any.only[IOCap]}] => (x: Console^{C}) => x)
    val _: Console^{con} = f.value[{con}](con)

// Bound with a restricted capability
object ClsBoundPath:
  trait IOCap extends SharedCapability, Classifier
  class Box[A](val value: A)
  def test(c: Object^, y: Object^{c.only[IOCap]}) =
    val f = Box([C^ <: {c.only[IOCap]}] => (x: Object^{C}) => x)
    val _: Object^{c.only[IOCap]} = f.value[{c.only[IOCap]}](y)

// `C.only` and `C.except` in parameter types
object ClsOnlyExcept:
  trait IOCap extends SharedCapability, Classifier
  class Console extends IOCap
  class Label extends Control
  class Box[A](val value: A)
  def test(con: Console^, l: Label^) =
    val f = Box([C^] => (x: Object^{C.only[Control]}) => x)
    val _: Object^{l} = f.value[{con, l}](l)
    val g = Box([C^] => (a: Object^{C}) => (b: Object^{C.except[Control]}) => b)
    val _: Object^{con} = g.value[{con, l}](l)(con)

// Nested in two containers
object ClsNested:
  class Label extends Control
  class Box[A](val value: A)
  def test(l: Label^) =
    val f = Box(Some([C^ <: {any.only[Control]}] => (x: Label^{C}) => x))
    val _: Label^{l} = f.value.get[{l}](l)

// Lambda value with a classifier bound, through `identity`
object ClsPolyValue:
  trait IOCap extends SharedCapability, Classifier
  class Console extends IOCap
  class Label extends Control
  class Box[A](val value: A)
  def test(con: Console^, l: Label^) =
    val p = [C^ <: {any.only[SharedCapability]}] => (x: Object^{C.only[IOCap]}) => x
    val f = identity(Box(p))
    val _: Object^{con} = f.value[{con, l}](con)

// Higher-order parameters with restricted results
object ClsTry:
  trait IOCap extends SharedCapability, Classifier
  class Console extends IOCap:
    def print(): Unit = ()
  class Label extends Control:
    def break(): Unit = ()
  class Box[A](val value: A)
  class Try[+T]
  object Try:
    def apply[T, C^](body: () ->{C} T): Try[T]^{C.only[Control]} = ???
  def test(con: Console^, l: Label^) =
    val f = Box([C^] => (body: () ->{C} Unit) => Try(body))
    val _: Try[Unit]^{l} = f.value[{con, l}](() => { con.print(); l.break() })
    val g = Box([C^ <: {any.only[Control]}] => (body: () ->{C} Unit) => body())
    g.value[{l}](() => l.break())

// `Unscoped` bound on a Mutable parameter
object ClsUnscoped:
  class Ref extends Mutable:
    var x = 0
    def get: Int = x
    update def put(y: Int): Unit = x = y
  class Box[A](val value: A)
  def test(r: Ref^) =
    val set = Box([C^ <: {any.only[Unscoped]}] => (x: Ref^{C}) => x.put(1))
    set.value[{r}](r)
    r.put(2)
    val get = Box([C^ <: {any.only[Unscoped]}] => (x: Ref^{C}) => x.get)
    val _: Int = get.value[{r.rd}](r)

// C mixed with a restricted outer capability
object ClsMixed:
  class Label extends Control:
    def break(): Unit = ()
  class Box[A](val value: A)
  def test(l: Label^, c: Object^, y: Object^{c.only[Control]}) =
    val f = Box([C^] => (x: Object^{C, c.only[Control]}) => x)
    val _: Object^{l, c.only[Control]} = f.value[{l}](y)
    val g = Box([C^] => (x: Label^{C}) => () => { l.break(); x.break() })
    val _: () ->{l} Unit = g.value[{l}](l)
