package aliasvariance

// Compiled in a separate run, so all of the definitions above are read from TASTy.
object Test:
  def abstractMember(h: HasAbstract) = summon[h.F[Any] <:< h.F[Int]]

  summon[Klass[Any] <:< Klass[Int]]
  summon[Defs.Op[Any] <:< Defs.Op[Int]]

  // declared contravariance of the transparent alias
  summon[Defs.Tr[Any] <:< Defs.Tr[Int]]
  // still holds by dealiasing to the covariant `List`
  summon[Defs.Tr[Int] <:< Defs.Tr[Any]]

  // `A` is invariant because `B` carries a variance annotation, so neither
  // argument position may be widened in `A`
  summon[Defs.Fn[Int, Int] <:< Defs.Fn[Int, Any]]
