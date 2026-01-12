import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Foo extends ExclusiveCapability, Classifier

// Testing with various DerivedCapabilities
class D
class C(val d: D)
class B(val c: C) extends Foo, Stateful:
  update def foo() = println("foo")
class A(consume val b: B^) extends Stateful:
  update def bar() = b.foo()
class A2(consume val b: B^) extends Stateful:
  update def bar() = b.foo()
class A3(consume var b: B^) extends Stateful:
  update def bar() = b.foo()
class A4(consume val b: A2^{any.only[Foo]}) extends Stateful: // FIXME needs to be classified as Foo, too
  update def bar() = b.b.foo()

// Test: Access nested fields (suffix paths) after consume
def testSuffixPaths =
  val d: D = D()
  val c: C = C(d)
  val b: B^ = B(c)
  val a1 = A(b)
  val b2: B^ = B(c)
  val a2 = A2(b2)
  val b3: B^ = B(c)
  val a3 = A3(b3)
  val b4: B^ = B(c)
  val a22 = A2(b4)
  //val a4 = A4(a22) // FIXME should work?
  val b5: B^{any.only[Foo]} = B(c)
  val a5 = A(b5)
  val b6: B^{any.only[Foo]} = B(c)
  val a222 = A2(b6)
  //val a6 = A4(a222) // FIXME should work?


  println(a1.b) // ok
  println(a2.b) // ok
  println(a3.b) // ok
  //println(a4.b) // ok needs fixing
  //println(a4.b.b) // ok needs fixing
  println(a5.b) // ok
 // println(a6.b) // ok needs fixing
 // println(a6.b.b) // ok needs fixing

  println(b) // error
  println(b2) // error
  println(b3) // error
  println(b4) // error
  println(b5) // error
  println(b6) // error
  //println(a222) // should error, but doesn't work yet