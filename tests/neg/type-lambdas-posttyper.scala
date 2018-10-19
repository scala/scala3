object Test extends App {

  trait Ord[X]

  type TL1 = [X <: Ord[X]] => (X, X)

  class C extends Ord[C]

  type T1 = TL1[Int] // error: Type argument Int does not conform to upper bound Test.Ord[LazyRef(Int)
  type T2 = TL1[C]   // OK

  class Ref[X](init: X) {
    var x: X = init
  }

  type TL3 = [+X] => Ref[X] // error: covariant type parameter X occurs in nonvariant position in Test.Ref[X]
  type TL4[-X] = X => X // error: contravariant type parameter X occurs in covariant position in X => X

  def f[F <: [+X] => Any](x: F[String]): F[Any] = x

  val sref = new Ref[String]("abc")
  val aref: Ref[Any] = f[TL3](sref)
  aref.x = 1
  val s: String = sref.x

}
