object TupleNestedPairsDirect:
  type TupleAlias = (Int, String)
  type PairAlias = Int *: String *: EmptyTuple
  type RefinedTuple = (Int, String) { type Marker = Int }

  val directTuple =
    summon[(Int, String, Boolean) <:< (Int *: String *: Boolean *: EmptyTuple)]

  val tupleAlias =
    summon[TupleAlias <:< (Int *: String *: EmptyTuple)]

  val refinedTuple =
    summon[RefinedTuple <:< (Int *: String *: EmptyTuple)]

  val explicitPairToTuple =
    summon[PairAlias <:< TupleAlias]
