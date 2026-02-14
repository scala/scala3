case class P(x: Int, y: Int)

object Test:

  def main(args: Array[String]): Unit =
    // p.copy(y = 7) means copy$default$1 for x (keep original), copy gets y=7
    // The macro rewrites copy$default$2 -> copy$default$1
    // So p.copy(y = 7) effectively becomes p.copy(x = 3, y = 7) but with
    // the first default changed: the second arg keeps its explicit value 7,
    // the first arg now uses copy$default$1 which is p.x = 3
    val result = TestMacro.swapFirstArg {
      val p = P(3, 4)
      p.copy(y = 7)
    }
    assert(result == P(3, 7), s"Expected P(3, 7) but got $result")
    println("Test passed!")
