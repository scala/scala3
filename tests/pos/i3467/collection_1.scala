package collection

abstract class WithFilter[+A, +CC[_]]

trait IndexedSeq[+A] extends Any with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]

trait IndexedSeqOps[+A, +CC[X] <: IndexedSeq[X], +C] extends Any {
  def withFilter(p: A => Boolean): WithFilter[A, CC] = ???
}

package immutable {
  trait IndexedSeq[+A] extends collection.IndexedSeq[A] with collection.IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]
}

object ArrayOps {
  abstract class WithFilter[A] extends collection.WithFilter[A, immutable.IndexedSeq]
}

class ArrayOps[A](val xs: Array[A]) extends AnyVal with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]] {
  override def withFilter(p: A => Boolean): ArrayOps.WithFilter[A] = ???
}
