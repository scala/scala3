import collection.mutable.UnrolledBuffer



object Test {

  def main(args: Array[String]): Unit = {
    val buf = UnrolledBuffer(1 to 50*)
    val dub = buf ++ buf

    println(dub)
  }

}
