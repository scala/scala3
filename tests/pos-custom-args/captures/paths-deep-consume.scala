import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.cap

class B

class A(consume val b: B^):
  val bb: B^ = B()

class C(consume val a: A^):
  val aa: A^ = A(B())

// Test deep nested access paths
def testDeepPaths =
  val b: B^ = B()
  val a = A(b)
  val c = C(a)

  // All these should work - accessing through ownership chain
  println(c.a)       // c owns a
  println(c.a.b)     // c owns a, a owns b
  println(c.a.bb)    // c owns a, accessing other field
  println(c.aa)      // accessing other field of c
  println(c.aa.b)    // accessing nested field through c.aa
