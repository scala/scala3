object Test {
  import scalatest._

  def neverRuns(f: => Unit): Boolean = true

  def main(args: Array[String]): Unit = {
    assert(this.neverRuns(sys.error("Sad times 1")))
  }
}