object Test {
  given extension[A](a: A) { def <<< : A = a }
  given extension (b: Int) { def <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
