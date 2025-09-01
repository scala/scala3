trait Eq[A] {
  def eqv(a1: A, a2: A): Boolean
}

given stringEq: Eq[String] {
  def eqv(a1: String, a2: String) = a1 == a2
}

abstract class Newtype[Src] {
  opaque type Type = Src

  protected final def derive[F[_]](using ev: F[Src]): F[Type] = ev
}

object Sample extends Newtype[String] {
  given eq: Eq[Type] = derive
}