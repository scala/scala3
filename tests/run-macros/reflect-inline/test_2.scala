import api.*

object Test {
  def main(args: Array[String]): Unit = {
    inline val a = "5"
    assert(typeChecks("|1 + 1".stripMargin))
    assert(scala.compiletime.testing.typeChecks("|1 + 1".stripMargin))
    assert(("|3 + " + a).stripMargin == "3 + 5")
  }
}
