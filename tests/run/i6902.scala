object Test {
  extension of [A](a: A) { def <<< : A = a }
  extension of (b: Int) { def <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
