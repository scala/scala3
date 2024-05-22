//> using options -source 3.5-migration

final class StrictEqual[V]
final class Less[V]
type LessEqual[V] = Less[V] | StrictEqual[V]

object TapirCodecIron:
  trait ValidatorForPredicate[Value, Predicate]
  trait PrimitiveValidatorForPredicate[Value, Predicate]
      extends ValidatorForPredicate[Value, Predicate]

  given validatorForLessEqual[N: Numeric, NM <: N](using
      ValueOf[NM]
  ): PrimitiveValidatorForPredicate[N, LessEqual[NM]] = ???
  given validatorForDescribedOr[N, P](using
      IsDescription[P]
  ): ValidatorForPredicate[N, P] = ???

  trait IsDescription[A]
  object IsDescription:
    given derived[A]: IsDescription[A] = ???

@main def Test = {
  import TapirCodecIron.{*, given}
  type IntConstraint = LessEqual[3]
  summon[ValidatorForPredicate[Int, IntConstraint]] // warn
}