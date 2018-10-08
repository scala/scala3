object Test extends App {

  trait Ord[X]

  type TL1 = [X <: Ord[X]] => (X, X) // OK
  type TL2 = [X >: Ord[X]] => (X, X) // error

  class C extends Ord[C]

  type T1 = TL1[Int] // will be discovered later
  type T2 = TL1[C]   // OK

  class Ref[X](init: X) {
    var x: X = init
  }

  type TL3 = [+X] => Ref[X]

  def f[F <: [+X] => Any](x: F[String]): F[Any] = x

  val sref = new Ref[String]("abc")
  val aref: Ref[Any] = f[TL3](sref)
  aref.x = 1
  val s: String = sref.x




}
