object Test {
  given [A] { def (a: A) <<< : A = a }
  given { def (b: Int) <<<< : Int = b }

  def main(args: Array[String]): Unit = {
    1.<<<
    1.<<<<
  }
}
