import language.experimental.captureChecking

class A:
  val b: A = this // error
  val f: A^ = this

def test(a: A^) =
  val c1: A = a      // error
  val c2: A = a.b    // ok
  val c3: A = a.f    // error
  val f1: () -> Unit = () => println(a) // error
  val f2: () -> Unit = () => println(a.b)  // ok!
  val f3: () -> Unit = () => println(a.f) // error