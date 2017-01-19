

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(new OfInt().iterator.next())
  }
}

class OfInt extends IndexedSeqLike2[Int] {
  def foo(index: Int): Int = 42
}

trait IndexedSeqLike2[A] {

  def foo(index: Int): A
  def iterator: Elements[A] = new Elements[A]

  class Elements[B >: A] extends Iterator2[B] {
    def next(): B = foo(0)
  }
}

trait Iterator2[A] {
  def next(): A
}
