class C {

  private type T = E
  private type Tok = D
  private val p: C = new C

  def f1(x: T): Unit = () // error
  def f1(x: Tok): Unit = () // ok
  def f2(x: p.D): Unit = () // error

  val v1: T = ??? // error
  val v2: p.D = ??? // error

  type U1[X <: T] // error
  type U2 = T // error

  private class E {
    def f1ok(x: T): Unit = () // ok
    def f2ok(x: p.D): Unit = () // ok

    val v1ok: T = ??? // ok
    val v2ok: p.D = ??? // ok

    type U1ok[X <: T] //ok
    type U2ok = T //ok
  }

  class D extends E { // error
    def f1(x: T): Unit = () // error
    def f2(x: p.D): Unit = () // error

    val v1: T = ??? // error
    val v2: p.D = ??? // error

    type U1[X <: T] // error
    type U2 = T // error
  }

  class F(x: T) // error

  class G private (x: T) // ok

  private trait U

  class H extends U // error

}
