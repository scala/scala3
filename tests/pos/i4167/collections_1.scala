package collection

trait Seq[+A] extends SeqOps[A, Seq, Seq[A]]
trait SeqOps[+A, +CC[_], +C] extends Any

package immutable {
  trait Seq[+A] extends collection.Seq[A] with SeqOps[A, Seq, Seq[A]]
  trait SeqOps[+A, +CC[_], +C] extends collection.SeqOps[A, CC, C]
}

class StringOps extends collection.SeqOps[Char, immutable.Seq, String]