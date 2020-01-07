object Test {
  extension of [A](a: A) with { def <<< : A = a }
  extension of (b: Int) with { def <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
