object Test {
  extension [A](a: A) { def <<< : A = a }
  extension (b: Int) { def <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
