
trait Sub[R, T >: R]
given [R, T >: R]: Sub[R, T] with {}

trait Candidate[-R]:
  type OutP
given [P]: Candidate[Option[P]] with
  type OutP = P

extension [L](lhs: L)
  def ^^^[P](rhs: Option[P])
            (using es: Sub[lhs.type, Any])
            (using c: Candidate[L])
            (using check: c.type <:< Any): Option[c.OutP] = ???

val x: Option[Boolean] = ???

val z1 = x ^^^ x // Ok
val z2 = z1 ^^^ x // Ok
val zz = ^^^[Option[Boolean]](x ^^^ x)(x) // Ok

val zzz = x ^^^ x ^^^ x // Error before changes
