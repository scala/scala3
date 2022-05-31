// This should be a neg test once level checking is re-enabled.

trait E[F[_]] {
  type T
  val value: F[T]
}

object E {
  def apply[F[_], T1](value1: F[T1]) = new E[F] {
    type T = T1
    val value = value1
  }
}

val a: Option[E[Ordering]] = Option(E(Ordering[Int]))
val _ = a.map(it => E(it.value)) // there should be an error here

