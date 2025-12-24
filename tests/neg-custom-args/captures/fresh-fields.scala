open class A:
  def a: A^ = A()               // no fresh contribution since `a` is a method
  val as: List[A^] = List(A())  // no fresh contribution since A^ is boxed

class B:
  private val b: A^ = A()   // contributes fresh

class C extends B  // contributes fresh via B

class D:
  private val f: () -> A = () => new A()
  private val b: () -> A^ = f   // contributes fresh

class Shared extends caps.SharedCapability

trait E:
  private val e: Shared = Shared()

class F extends B, E

def test() =
  val a = A()
  val _: A = a // ok
  val b = B()
  val _: B = b // error
  val c = C()
  val _: C = c // error
  val d = D()
  val _: D = d // error
  val e = new E() {}
  val _: E = e // error
  val f = F()
  val _: F = f // error
