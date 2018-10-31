package object scala {
  // needed for some reasons
  type Throwable = java.lang.Throwable
  type IndexOutOfBoundsException = java.lang.IndexOutOfBoundsException
  type List[+A] = scala.collection.immutable.List[A]
  type Iterable[+A] = scala.collection.Iterable[A]

  type Seq[A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq
}
