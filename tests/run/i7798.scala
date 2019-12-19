object Test {
  inline def meth[R <: Int]: R = 3.asInstanceOf[R]
  def main(args: Array[String]): Unit = {
    val t: Int = meth
  }
}
