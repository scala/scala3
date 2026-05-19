//> using options -Xcheck-macros

case class P(x: Int, y: Int)

object Test:

  def main(args: Array[String]): Unit =
    val result = TestMacro.identityTreeMap {
      val p = P(3, 4)
      p.copy(y = 7)
    }
    assert(result == P(3, 7))
    println("Test passed!")
