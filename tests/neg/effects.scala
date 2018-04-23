
import Effect.isPure
object Test {

  def f(implicit x: CanThrow[NullPointerException]) = ()

  f // error

  val x1: Any = Effect.canThrowNPE // error
  val x2: Any = Effect.isImpure: Impure // error

  val y: Pure = ??? // error

  def g1[T <: Pure] = () // OK
  def g2[T >: Impure] = () // OK
  def h1[T >: Impure <: Any]  = () // error
  def h2[T >: Nothing <: Pure] = () // error

  type P = Pure

  type I <: Impure

  type C[-E <: Throwable] = CanThrow[E]

  def z1: P & Nothing = ???  // error // error
  def z2: Any | I  = ???   // error

  def z3: C[AssertionError] & Nothing = ???  // error // error

  def z4: CanThrow[AssertionError] = ??? // error
  def z5: C[AssertionError] = ??? // error

  if (true) 1 else Effect.isImpure // error

  1 match {
    case 1 => Effect.isImpure
    case _ => 0 // error
  }

  try {
    1
  }
  catch {
    case ex: Error => Effect.isImpure // error
  }

  val x: Pure = Effect.isImpure // error
  var x = Effect.isImpure // error

  def f2(x: Int): C[AssertionError] = Effect.isImpure // error
  def f3(x: Int) = Effect.isImpure // error

  def foo() = {
    val effVal: C[AssertionError] = Effect.isImpure // ok
    def effMeth(): C[AssertionError] = Effect.isImpure // error
  }

  val effVal: C[AssertionError] = Effect.isImpure // error
}
