




object Test {

  class Empty extends Traversable[Nothing] {
    def foreach[U](f: Nothing => U): Unit = {}
  }

  def main(args: Array[String]): Unit = {
    val t = new Empty
    t.toStream
  }

}
