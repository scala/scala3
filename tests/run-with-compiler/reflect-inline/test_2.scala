import api._

object Test {
  def main(args: Array[String]): Unit = {
    assert(typeChecks("1 + 1".strip))
    assert(scala.testing.typeChecks("1 + 1".strip))
  }
}