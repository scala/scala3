

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(new OfInt().iterator.next())
  }
}

class OfInt extends IndexedSeqLike2 {
  def foo(index: Int): Int = 42
}

trait IndexedSeqLike2 {

  def foo(index: Int): Int
  def iterator: Elements = new Elements

  class Elements extends Iterator2[Int] {
    def next(): Int = foo(0)
  }
}

trait Iterator2[A] {
  def next(): Int
}
