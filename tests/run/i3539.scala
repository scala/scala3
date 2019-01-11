object Test {
  def main(args: Array[String]): Unit = {
    val i2s = (x: Int) => ""
    assert(i2s.asInstanceOf.asInstanceOf[AnyRef|Null => String].apply(null) == "")
    val i2i = (x: Int) => x + 1
    assert(i2i.asInstanceOf[AnyRef|Null => Int].apply(null) == 1)
  }
}

class Test {

  asInstanceOf[Nothing].asInstanceOf[AnyRef|Null => String]

  asInstanceOf[Nothing].asInstanceOf[AnyRef|Null => String].apply(null)

}
