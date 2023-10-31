object Try1 {
  // type-level naturals
  sealed trait TNat
  sealed trait TZero extends TNat
  sealed trait TSucc[T] extends TNat

  //reflect TNat at the value level; given n: Nat[T], n and T represent the same number.
  //This is what Haskellers call "singleton types" and took from Omega.
  // The point is that we can use such a number both at compile-time and at runtime,
  // but of course the runtime representation is not erased.
  //And this constrains the type argument of `Nat` to be of type `TNat` — though you could add bounds for it.

  sealed trait Nat[+T]
  case class Zero() extends Nat[TZero]
  case class Succ[T](n: Nat[T]) extends Nat[TSucc[T]]

  //We can index Vect with the types of value-level Nat, but this is a bit overkill. Still, no warnings.
  sealed trait Vect[N <: Nat[?], +T]
  case class VN[T]() extends Vect[Zero, T]
  case class VC[T, N <: Nat[?]](x: T, xs: Vect[N, T]) extends Vect[Succ[N], T]

  object Test {
    def foo[N <: Nat[?], A, B](v1: Vect[N, A], v2: Vect[N, B]) =
      (v1, v2) match {
        case (VN(), VN())           => 1
        case (VC(x, xs), VC(y, ys)) => 2
      }
  }
}

//Since we didn't need value-level numbers, let's drop them:
object Try2 {
  sealed trait TNat
  sealed trait TZero extends TNat
  sealed trait TSucc[T] extends TNat

  sealed trait Vect[N <: TNat, +T]
  case class VN[T]() extends Vect[TZero, T]
  case class VC[T, N <: TNat](x: T, xs: Vect[N, T]) extends Vect[TSucc[N], T]

  object Test {
    def foo[N <: TNat, A, B](v1: Vect[N, A], v2: Vect[N, B]) =
      (v1, v2) match {
        case (VN(), VN())           => 1
        case (VC(x, xs), VC(y, ys)) => 2
      }
  }
}

//Same as Try2, but now `Vect` is covariant in `N` so we get the warning you described.
object Try3 {
  sealed trait TNat
  sealed trait TZero extends TNat
  sealed trait TSucc[T] extends TNat

  sealed trait Vect[+N <: TNat, +T]
  case class VN[T]() extends Vect[TZero, T]
  case class VC[T, N <: TNat](x: T, xs: Vect[N, T]) extends Vect[TSucc[N], T]

  object Test {
    def foo[N <: TNat, A, B](v1: Vect[N, A], v2: Vect[N, B]) =
      //Warnings expected here!
      (v1, v2) match {
        case (VN(), VN())           => 1
        case (VC(x, xs), VC(y, ys)) => 2
      }
    //a call-site which would cause a MatchError (maybe that error should be tested)
    def bar = foo[TZero | TSucc[?], Int, String](VN(), VC("", VN()))
  }
}

//Same as Try3, but now `Vect` is invariant
object Try4 {
  sealed trait TNat
  sealed trait TZero extends TNat
  sealed trait TSucc[T] extends TNat

  sealed trait Vect[N <: TNat, +T]
  case class VN[T]() extends Vect[TZero, T]
  case class VC[T, N <: TNat](x: T, xs: Vect[N, T]) extends Vect[TSucc[N], T]

  object Test {
    def foo[N <: TNat, A, B](v1: Vect[N, A], v2: Vect[N, B]) =
      (v1, v2) match {
        case (VN(), VN())           => 1
        case (VC(x, xs), VC(y, ys)) => 2
      }
  }
}
