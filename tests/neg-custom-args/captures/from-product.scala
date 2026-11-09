// Simulates fromProduct without having to go through typeclass derivation
class C
class D extends caps.Pure

trait ProdMirror:
  type T
  def fromProd(x: Product): T^ = ???

object TestNormal extends ProdMirror:

  type T = C

  def test(x: Product): (C^, C^) =
    (fromProd(x), fromProd(x))  // ok

  def test2(x: Product^): (C^, C^) =
    (fromProd(x), fromProd(x))  // error // error

object TestPure extends ProdMirror:
  type T = D

  def test2(x: Product): (D, D) =  // ok
    (fromProd(x), fromProd(x))
