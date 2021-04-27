object Test {
  def main(args: Array[String]): Unit = {
    println(seq)
    val s: Seq[Int] = seq
    println(s)
    println(List(seq*))
    println(seq(1))
  }
}
