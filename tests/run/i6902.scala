object Test {
  given [A](a: A) extended with { def <<< : A = a }
  given (b: Int) extended with { def <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
