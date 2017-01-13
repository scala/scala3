

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(new OfInt().iterator.next())
  }
}

class OfInt extends IndexedSeqLike2[Int] {
  def apply(index: Int): Int = 42
}

trait IndexedSeqLike2[A] {

  def apply(index: Int): A
  def iterator: Elements = new Elements

  class Elements extends Iterator2[A] {
    def next(): A = apply(0)
  }
}

trait Iterator2[A] {
  def next(): A
}
