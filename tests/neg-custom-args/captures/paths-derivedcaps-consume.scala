import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Foo extends ExclusiveCapability, Classifier

// Testing with various DerivedCapabilities
class D
class C(val d: D)
class B(val c: C) extends Foo, Mutable:
  update def foo() = println("foo")
class A(consume val b: B^) extends Mutable:
  update def bar() = b.foo()
class A2(consume val b: B^)
class A3(consume var b: B^)
class A4(consume val b: A2^{cap.only[Foo]})

// Test: Access nested fields (suffix paths) after consume
def testSuffixPaths =
  val d: D = D()
  val c: C = C(d)
  val b: B^ = B(c)
  val a1 = A(b)
  val b2: B^ = B(c)
  val a2 = A2(b2) // the difference is that we pass b2.rd
  val b3: B^ = B(c)
  val a3 = A3(b3)
  val b4: B^ = B(c)
  val a22 = A2(b4)
  val a4 = A4(a22)
  val b5: B^{cap.only[Foo]} = B(c)
  val a5 = A(b5)
  val b6: B^{cap.only[Foo]} = B(c)
  val a222 = A2(b6)
  val a6 = A4(a222)


  println(a1.b) // ok
  println(a2.b) // ok
  println(a3.b) // ok
  println(a4.b) // ok
  println(a4.b.b) // ok
  println(a5.b) // ok
  println(a6.b) // ok
  println(a6.b.b) // ok

  println(b) // error
  println(b2) // error
  println(b3) // error
  println(b4) // error
  //println(b5) // should error (currently accepted!!!)
  //println(b6) // should error (currently accepted!!!)
  //println(a222) // should error (currently accepted!!!)