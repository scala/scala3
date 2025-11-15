import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.cap

class B

class A(consume val b: B^):
  val bb: B^ = B()

class C(consume val a: A^):
  val aa: A^ = A(B())

// Test: After consuming b into a, then consuming a into c,
// we should be able to access c.a.b (nested field selection)
def testNestedAccess =
  val b: B^ = B()
  val a = A(b)
  println(a.b)    // OK - a consumed b, accessing through a
  val c = C(a)
  println(c.a)    // Should be OK - c consumed a, accessing through c
  println(c.a.b)  // Should be OK - accessing b through c.a, where c owns a and a owns b

// Test: The deeper path c.a.b should work even though a was consumed
def testDeepPath =
  val b: B^ = B()
  val a = A(b)
  val c = C(a)
  // At this point:
  // - b was consumed by a (so we can't use b directly)
  // - a was consumed by c (so we can't use a directly)
  // But we should be able to access:
  println(c.a.b)  // OK - full path through ownership chain
  println(c.a.bb) // OK - other field of c.a
