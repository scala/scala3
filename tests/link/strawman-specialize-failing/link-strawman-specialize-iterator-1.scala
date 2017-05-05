
object Test {
  def main(args: Array[String]): Unit = {
    println(new Foo("hello").next)
  }
}

class Foo[T](e: T) extends strawman.collection.Iterator[T] {
  def hasNext = true
  def next: T = e
}