
class A:
  protected class B

// This fails to compile, as expected
val x = A().B() // error

object C:
  protected val p = "protected"
  protected def getString() = "Hello!"
  protected class D:
    def d = D() // ok

// This fails to compile
// val y = C.p

// And this also fails to compile
// val z = C.getString()

// However, this compiles just fine.
val alpha = C.D() // error
val beta = new C.D() // error
