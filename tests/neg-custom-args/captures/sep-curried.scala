import language.experimental.captureChecking
import caps.*

class Ref[T](init: T) extends Mutable:
  private var value: T = init
  def get: T = value
  mut def set(newValue: T): Unit = value = newValue

// a library function that assumes that a and b MUST BE separate
def swap[T](a: Ref[Int]^, b: Ref[Int]^): Unit = ???

def test0(): Unit =
  val a: Ref[Int]^ = Ref(0)
  def foo(x: Ref[Int]^)(y: Ref[Int]^{a}): Unit =
    swap(x, y)
  foo(a)(a)  // error

def test1(): Unit =
  val a: Ref[Int]^ = Ref(0)
  val foo: (x: Ref[Int]^) -> (y: Ref[Int]^{a}) ->{x} Unit =
    x => y => swap(x, y)
  val f: (y: Ref[Int]^{a}) ->{a} Unit = foo(a) // error
  f(a)

def test2(): Unit =
  val a: Ref[Int]^ = Ref(0)
  val foo: (x: Ref[Int]^) -> (y: Ref[Int]^{a}) ->{x} Unit =
    x => y => swap(x, y)
  foo(a)(a)  // error

def test3(): Unit =
  val a: Ref[Int]^ = Ref(0)
  val foo: (x: Ref[Int]^) -> (y: Ref[Int]^) ->{x} Unit =
    x => y => swap(x, y)
  foo(a)(a)  // error

def test4(): Unit =
  val a: Ref[Int]^ = Ref(0)
  val foo: (x: Ref[Int]^) -> (y: Ref[Int]^) ->{x} Unit =
    x => y => swap(x, y)
  val f = foo(a)
  f(a) // error

def test5(): Unit =
  val a: Ref[Int]^ = Ref(0)
  val foo: (x: Ref[Int]^) -> (y: Ref[Int]^) ->{x} Unit =
    x => y => swap(x, y)
  val f: (y: Ref[Int]^{a}) ->{a} Unit = foo(a) // error
  f(a)
