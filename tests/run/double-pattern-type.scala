case class C1(i: String, s: Int) { def isEmpty = false; def get = ("EMPTY", -1) }
case class C2(i: String, s: String) { def isEmpty = false; def get = (-1, -2, -3) }

object Test {
  def main(args: Array[String]): Unit = {
    // When both Product and name based patterns with same arity are available,
    // we follow scalac and silently use the Product one:

    val c1 = C1("s", 0)
    c1 match {
      case C1(a, b) =>
        assert(a == "s")
        assert(b == 0)
    }

    // When the size differ, both are patterns become usable:

    val c2 = C2("a", "b")
    c2 match {
      case C2(a, b) =>
        assert(a == "a")
        assert(b == "b")
    }

    c2 match {
      case C2(a, b, c) =>
        assert(a == -1)
        assert(b == -2)
        assert(c == -3)
    }

    // Interestingly things also compile with a single pattern, in which case
    // the tuple returned by get is binded to `a`:

    c2 match {
      case C2(a) =>
        assert(a == (-1, -2, -3))
    }
  }
}
