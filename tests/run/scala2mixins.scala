import scala.collection.IndexedSeqOptimized
import scala.collection.mutable.Builder

object Test {
  class Name extends Seq[Int]
    with IndexedSeqOptimized[Int, Name] {
    val underlying = 0 to 10
    def length: Int = underlying.length

    def apply(idx: Int): Int = underlying(idx)

    override protected[this] def newBuilder: Builder[Int, Name] = ???

    override def seq = toCollection(this)

  }
  def main(args: Array[String]): Unit = {
    val n = new Name
    // need to make sure that super accessors were emitted
    // ends with calls super.endsWith if argument is not an IndexedSeq
    assert(n.endsWith(10 :: Nil))
  }
}
