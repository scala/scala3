package object scala {
  // needed for some reasons
  type Throwable = java.lang.Throwable
  type IndexOutOfBoundsException = java.lang.IndexOutOfBoundsException

  type Seq[A] = scala.collection.Seq[A]
  val Seq = scala.collection.Seq
}
