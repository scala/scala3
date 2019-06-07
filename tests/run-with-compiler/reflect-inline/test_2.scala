import api._

object Test {
  def main(args: Array[String]): Unit = {
    val a: String = "5"
    assert(typeChecks("|1 + 1".stripMargin))
    assert(scala.testing.typeChecks("|1 + 1".stripMargin))
    assert(("|3 + " + a).stripMargin == "3 + 5")
  }
}