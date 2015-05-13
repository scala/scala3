object Test {
  def main(args: Array[String]): Unit = {
    val set = collection.mutable.Set("1", null, "3").par
    assert( set exists (_ eq null) )
  }
}
