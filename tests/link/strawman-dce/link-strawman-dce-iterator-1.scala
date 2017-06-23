
object Test {
  def main(args: Array[String]): Unit = {
    println(new Foo().next)
  }
}

class Foo extends strawman.collection.Iterator[String] {
  def hasNext = true
  def next = "hello"
}