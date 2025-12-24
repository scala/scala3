import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Create a deep nesting structure for testing suffix paths
class D
class C(val d: D^)
class B(val c: C^)
class A(consume val b: B^) extends Separate

// Test 1: Simple suffix paths after consume
def test1 =
  val d: D^ = D()
  val c: C^ = C(d)
  val b: B^ = B(c)
  val a = A(b)

  // After 'a' consumes 'b', access nested fields:
  println(a.b)       // OK - consumed field
  println(a.b.c)     // OK - suffix: .c after consumed field
  println(a.b.c.d)   // OK - suffix: .c.d after consumed field

// Test 2: Non-consume wrapper with consumed inner field
class Holder(val a: A^)  // Note: NOT consume

def test2 =
  val d: D^ = D()
  val c: C^ = C(d)
  val b: B^ = B(c)
  val a = A(b)
  val holder = Holder(a)

  // Access through holder.a.b where b was consumed by a:
  println(holder.a)         // OK - not consumed
  println(holder.a.b)       // OK - accessing consumed field through prefix
  println(holder.a.b.c)     // OK - suffix .c
  println(holder.a.b.c.d)   // OK - suffix .c.d