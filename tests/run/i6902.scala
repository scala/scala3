object Test {
  extension on [A](a: A) { def <<< : A = a }
  extension on (b: Int) { def <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
