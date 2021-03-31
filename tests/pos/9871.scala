object Test {
  type IsTypeInTuple[T, Tup <: Tuple] = Tup match {
    case EmptyTuple => false
    case T *: ts => true
    case _ *: ts => IsTypeInTuple[T, ts]
  }
  summon[(Int *: String *: EmptyTuple) =:= (Int, String)] //they are the same
  summon[IsTypeInTuple[String, Int *: String *: EmptyTuple] =:= true] //compiles
  summon[IsTypeInTuple[String, (Int, String)] =:= true] //doesn't compile
}
