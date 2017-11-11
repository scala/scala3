object Test {
  // shapeless.Coproduct
  sealed trait Coproduct extends Product with Serializable
  sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
  final case class Inl[+H, +T <: Coproduct](head : H) extends :+:[H, T]
  final case class Inr[+H, +T <: Coproduct](tail : T) extends :+:[H, T]
  sealed trait CNil extends Coproduct

  // Note that this only appears when T is a type parameter. Replaying T with
  // a concrete type (CNil or another :+:) leads to accurate warnnings
  def f[T <: Coproduct](fa: Int :+: T) =
    fa match {
      case Inl(x) => 1
      case Inr(x) => 2 // Dotty thinks this unreachable, but it is!
    }

  def main(args: Array[String]): Unit =
    assert(f(Inr(Inl(-1))) == 2) // Example of reachability
}