object Test {
  type FieldType2[K, +V] = V with KeyTag2[K, V]
  trait KeyTag2[K, +V] extends Any

  type WrapUpper = Tuple
  type Wrap[A] = Tuple1[A]

  type Extract[A <: WrapUpper] = A match {
    case Wrap[h] => h
  }

  summon[Extract[Wrap[FieldType2["foo", Int]]] =:= FieldType2["foo", Int]] // error
  //                                                                     ^
  // Cannot prove that Main.Extract[Tuple1[Main.FieldType2[("foo" : String), Int]]] =:= Main.FieldType2[("foo" : String), Int].
  //
  // Note: a match type could not be fully reduced:
  //
  //   trying to reduce  Main.Extract[Tuple1[Main.FieldType2[("foo" : String), Int]]]
  //   failed since selector  Tuple1[Main.FieldType2[("foo" : String), Int]]
  //   is uninhabited.
}
