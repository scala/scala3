import scala.collection._

class MySeq[A](private val underlying: Seq[A]) // error
  extends Seq[A]
  with SeqOps[A, MySeq, MySeq[A]] {
  def apply(n: Int) = underlying(n)
  def iterator = underlying.iterator
  def length = underlying.size
}
