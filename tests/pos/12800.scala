object Test {
  type FieldType2[K, +V] = V with KeyTag2[K, V]
  trait KeyTag2[K, +V] extends Any

  type WrapUpper = Tuple
  type Wrap[A] = Tuple1[A]

  type Extract[A <: WrapUpper] = A match {
    case Wrap[h] => h
  }

  summon[Extract[Wrap[FieldType2["foo", Int]]] =:= FieldType2["foo", Int]]

  // This used to cause an error because `Tuple1[FieldType2["foo", Int]]` was
  // "provablyEmpty". Since we switched to testing the combination of
  // `scrut <: pattern` *and* `provablyDisjoint(scrut, pattern)` instead, this
  // particular example compiles, because `FieldType2["foo", Int]` is not
  // `provablyDisjoint` from `h` (`Any`).
}
