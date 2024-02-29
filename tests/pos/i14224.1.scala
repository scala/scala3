//> using options -Werror

// Derived from the extensive test in the gist in i14224
// Minimising to the false positive in SealedTrait1.either

sealed trait Foo[A, A1 <: A]
final case class Bar[A, A1 <: A](value: A1) extends Foo[A, A1]

class Main:
  def test[A, A1 <: A](foo: Foo[A, A1]): A1 = foo match
    case Bar(v) => v
