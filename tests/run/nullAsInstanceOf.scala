object Test {
  def main(args: Array[String]): Unit = {
    val u = null.asInstanceOf[Unit]
    val b = null.asInstanceOf[Byte]
    val c = null.asInstanceOf[Char]
    val s = null.asInstanceOf[Short]
    val i = null.asInstanceOf[Int]
    val l = null.asInstanceOf[Long]
    val f = null.asInstanceOf[Float]
    val d = null.asInstanceOf[Double]
    val str = null.asInstanceOf[String]

    println(u)
    println(b)
    println(c)
    println(s)
    println(i)
    println(l)
    println(f)
    println(d)
    println(str)
  }
}
