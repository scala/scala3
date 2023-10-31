type Ext1[S] = S match {
  case Seq[t] => t
}
type Ext2[S] = S match {
  case Seq[?] => Int
}
type Ext3[S] = S match {
  case Array[t] => t
}
type Ext4[S] = S match {
  case Array[?] => Int
}
def foo[T <: Seq[Any], A <: Array[B], B] =
  summon[Ext1[T] =:= T]    // error
  summon[Ext2[T] =:= Int]  // ok
  summon[Ext3[A] =:= B]    // ok
  summon[Ext4[A] =:= Int]  // ok