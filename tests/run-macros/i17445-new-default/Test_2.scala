case class P(x: Int, y: Int)

object Test:

  def main(args: Array[String]): Unit =
    // p.copy(y = 7) generates copy$default$1 for x (keeps p.x = 3).
    // The macro rewrites copy$default$1 -> copy$default$2,
    // so x's value becomes p.y (= 4) instead of p.x (= 3).
    // Result: p.copy(4, 7) = P(4, 7)
    val result = TestMacro.swapDefault {
      val p = P(3, 4)
      p.copy(y = 7)
    }
    assert(result == P(4, 7), s"Expected P(4, 7) but got $result")
    println("Test passed!")
