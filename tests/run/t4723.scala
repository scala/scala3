


object Test {
  def main(args: Array[String]): Unit = {
    assert(Nil == collection.parallel.ParSeq())
    assert(collection.parallel.ParSeq() == Nil)
  }
}
