trait Ctx1:
  type T
  val x: T
  val y: T

trait Ctx2:
  type T
  val x: T
  val y: T

trait A
trait B

def h(x: Boolean): A ?=> B ?=> (A, B) =
  (summon[A], summon[B]) // OK

def g(x: Boolean): (c1: Ctx1) ?=> Ctx2 ?=> (c1.T, Ctx2) =
  return ???  // error
  ???
