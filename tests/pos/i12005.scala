  trait Trait1[F[_]]
  trait Trait2[F[_], E]

  def apply[F[_]: Trait1, E](using h: Trait2[F, E]): Trait2[F, E] = summon

  def apply2[F[_]: Trait1, E](using h: Trait2[F, E]): Trait2[F, E] = implicitly
