import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.any
import scala.caps.Stateful

// Create a deeper nesting structure
class D()
class C(val d: D^)
class B(val c: C^)
class A(consume val b: B^) extends Stateful:
  update def use() = println("Using A")

// Test 1: Accessing nested fields through a consumed path
def testNestedFieldsAfterConsume =
  val d: D^ = D()
  val c: C^ = C(d)
  val b: B^ = B(c)
  val a = A(b)

  // After a consumed b, we should be able to access:
  println(a.b)       // OK - a owns b
  println(a.b.c)     // OK - accessing field of consumed b through a
  println(a.b.c.d)   // OK - deeper nesting through consumed path

// Test 2: Non-trivial prefix accessing a consumed field
class Container(consume val a: A^) extends Stateful:
  val other: A^ = A(B(C(D())))
  update def operate() = other.use()

class Outer(consume val container: Container^) extends Stateful:
  update def execute() = container.operate()

def testComplexPrefix =
  val d1: D^ = D()
  val c1: C^ = C(d1)
  val b1: B^ = B(c1)
  val a1 = A(b1)
  val container1 = Container(a1)
  val outer = Outer(container1)

  // Non-trivial prefix: outer.container.a (where 'a' was consumed by container)
  println(outer.container)            // OK - outer consumed container
  println(outer.container.a)          // OK - accessing consumed field through prefix
  println(outer.container.a.b)        // OK - and then its nested fields
  println(outer.container.a.b.c)      // OK - even deeper
  println(outer.container.a.b.c.d)    // OK - deepest level

  println(container1) // error
  println(a1)         // error

// Test 3: Multiple consume parameters with nested access
class Multi(consume val b1: B^, consume val b2: B^) extends Stateful:
  val b3: B^ = B(C(D()))
  update def combine() = ()

def testMultipleConsume =
  val b1: B^ = B(C(D()))
  val b2: B^ = B(C(D()))
  val multi = Multi(b1, b2)

  // All of these should work:
  println(multi.b1)           // OK
  println(multi.b1.c)         // OK - nested field of consumed b1
  println(multi.b1.c.d)       // OK - deeper nesting
  println(multi.b2)           // OK
  println(multi.b2.c)         // OK - nested field of consumed b2
  println(multi.b2.c.d)       // OK - deeper nesting
  println(multi.b3.c.d)       // OK - non-consumed field

  println(b1) // error
  println(b2) // error

// Test 4: Consume at multiple levels with complex paths
class Top(consume val outer: Outer^) extends Stateful:
  update def topAction() = outer.execute()

def testMultiLevelConsume =
  val d2: D^ = D()
  val c2: C^ = C(d2)
  val b2: B^ = B(c2)
  val a2 = A(b2)
  val container2 = Container(a2)
  val outer2 = Outer(container2)
  val top = Top(outer2)

  // Very deep path through multiple consume levels:
  println(top.outer)                      // OK - top consumed outer
  println(top.outer.container)            // OK - continue through path
  println(top.outer.container.a)          // OK - container consumed a
  println(top.outer.container.a.b)        // OK - a consumed b
  println(top.outer.container.a.b.c)      // OK - nested field
  println(top.outer.container.a.b.c.d)    // OK - deepest field

  println(a2) // error
  println(container2) // error
  println(outer2) // error
  println(top) // ok
