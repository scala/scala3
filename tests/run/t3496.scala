



// ticket #3496
object Test {

  def main(args: Array[String]): Unit = {
    val s = Stream.from(1)
    s.take(5)
    s.drop(5)
    s.splitAt(5)
  }

}
