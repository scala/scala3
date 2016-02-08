object Test {
  def mkArray(atype: Int): Array[_ <: AnyVal] = {
    (if (atype == 1) new Array[Int](10) else new Array[Float](10))
  }

  def main(args: Array[String]): Unit = {
    assert(mkArray(1).length == 10)
  }
}
