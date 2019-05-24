object Test extends App {

  trait Ord[X]

  type TL1 = [X <: Ord[X]] =>> (X, X) // OK
  type TL2 = [X >: Ord[X]] =>> (X, X) // error: illegal cyclic reference: lower bound Test.Ord[X] of type X refers back to the type itself

}
